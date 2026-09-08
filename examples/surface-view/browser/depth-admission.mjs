// Independent raw WebGL depth oracle. No ScalaFIM or Three.js rendering code.
// Usage: node depth-admission.mjs project-node_modules output.json [default|metal|no-antialias]
// Run the session browser ownership audit before and after invoking this file.
import { createServer } from 'node:http';
import { writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { pathToFileURL } from 'node:url';
const [dependencies, output, backend = 'default'] = process.argv.slice(2);
if (!dependencies || !output || !['default', 'metal', 'no-antialias'].includes(backend)) throw new Error('Expected dependency directory, output JSON, and renderer backend');
const { chromium } = await import(pathToFileURL(resolve(dependencies, 'playwright/index.mjs')));
const positions = [0.2984880387875073, 0.9149595922332878, -0.47781183593737453, -0.573554461565223, -0.021142485787464693, -0.4766295261365914, 0.20505161282218154, -0.32764376184935884, -0.4781022651073559, 0.15369809971540604, 0.7115875281530464, -0.4630327327636765, 0.34888187531136694, 1.2537314724282993, -0.4611825913627885, -0.1243654886001444, -0.2099440321898811, -0.4610836589246222];
const server = createServer((request, response) => {
  response.writeHead(200, { 'Content-Type': 'text/html' });
  response.end('<canvas width="8" height="8"></canvas>');
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
let browser;
try {
  browser = await chromium.launch({ headless: true, args: backend === 'metal' ? ['--use-gl=angle', '--use-angle=metal'] : [] });
  const page = await browser.newPage();
  await page.goto(`http://127.0.0.1:${server.address().port}/`);
  const result = await page.evaluate(({ positions, backend }) => {
    const gl = document.querySelector('canvas').getContext('webgl2', { antialias: backend !== 'no-antialias', alpha: false });
    if (!gl) throw new Error('WebGL2 unavailable');
    const shaders = [], buffers = [];
    const program = gl.createProgram();
    function shader(type, source) {
      const value = gl.createShader(type); shaders.push(value);
      gl.shaderSource(value, source); gl.compileShader(value);
      if (!gl.getShaderParameter(value, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(value));
      gl.attachShader(program, value);
    }
    try {
      shader(gl.VERTEX_SHADER, `#version 300 es
        precision highp float;
        in vec3 position; in vec3 color; out vec3 vColor;
        uniform bool constantDepth;
        void main() {
          gl_Position = vec4(position, 1.0);
          if (constantDepth) gl_Position.z = (color.r > 0.5 ? 0.261 : 0.269) * 2.0 - 1.0;
          vColor = color;
        }`);
      shader(gl.FRAGMENT_SHADER, `#version 300 es
        precision highp float;
        in vec3 vColor; out vec4 outColor;
        void main() { outColor = vec4(vColor, 1.0); }`);
      gl.linkProgram(program);
      if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(program));
      gl.useProgram(program);
      function attribute(name, data) {
        const buffer = gl.createBuffer(); buffers.push(buffer);
        gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(data), gl.STATIC_DRAW);
        const location = gl.getAttribLocation(program, name);
        gl.enableVertexAttribArray(location);
        gl.vertexAttribPointer(location, 3, gl.FLOAT, false, 0, 0);
      }
      attribute('position', positions);
      attribute('color', [...Array(3).fill([1, 0, 0]).flat(), ...Array(3).fill([0, 1, 0]).flat()]);
      gl.enable(gl.DEPTH_TEST); gl.depthFunc(gl.LESS); gl.clearDepth(1);
      gl.clearColor(1, 1, 1, 1); gl.viewport(0, 0, 8, 8);
      const rows = [];
      for (const constantDepth of [false, true]) for (const reverse of [false, true]) {
        gl.uniform1i(gl.getUniformLocation(program, 'constantDepth'), constantDepth);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        for (const start of reverse ? [3, 0] : [0, 3]) gl.drawArrays(gl.TRIANGLES, start, 3);
        const pixel = new Uint8Array(4);
        gl.readPixels(3, 4, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, pixel);
        rows.push({ constantDepth, reverse, pixel: Array.from(pixel), glError: gl.getError(),
          passed: pixel[0] === 255 && pixel[1] === 0 && pixel[2] === 0 && pixel[3] === 255 });
      }
      const debug = gl.getExtension('WEBGL_debug_renderer_info');
      return { samples: gl.getParameter(gl.SAMPLES), rows,
        renderer: debug ? gl.getParameter(debug.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER) };
    } finally {
      buffers.forEach(buffer => gl.deleteBuffer(buffer));
      shaders.forEach(shader => gl.deleteShader(shader));
      gl.deleteProgram(program);
      gl.getExtension('WEBGL_lose_context')?.loseContext();
    }
  }, { positions, backend });
  const receipt = { schema: 'scalafim.independent-webgl-depth.v1', browser: browser.version(), backend,
    positions, expectedPixel: [255, 0, 0, 255], ...result };
  await writeFile(output, JSON.stringify(receipt, null, 2) + '\n');
  console.log(JSON.stringify(receipt, null, 2));
} finally {
  if (browser) await browser.close();
  await new Promise(resolve => server.close(resolve));
}
