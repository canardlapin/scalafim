package com.sun.prism.impl;
import com.sun.javafx.geom.*;
public class TangentSpaceProbe {
 public static void main(String[] args) {
  int failures=0;
  Vec3f[] normals={new Vec3f(1,0,0),new Vec3f(0,1,0),new Vec3f(0,0,1),new Vec3f(-1,0,0),new Vec3f(0,-1,0),new Vec3f(0,0,-1)};
  for(Vec3f n:normals) {
   // Both, either, and neither UV derivative collapse on the normal plane.
   Vec3f axis=Math.abs(n.x)<0.5?new Vec3f(1,0,0):new Vec3f(0,1,0);
   Vec3f cross=new Vec3f();cross.cross(n,axis);
   for(int which=0;which<4;which++) {
    Vec3f[] frame={new Vec3f(n),which==0||which==1?new Vec3f(n):new Vec3f(axis),which==0||which==2?new Vec3f(n):new Vec3f(cross)};
    MeshUtil.fixTSpace(frame);Quat4f q=new Quat4f();MeshUtil.buildQuat(frame,q);
    boolean valid=true;
    for(Vec3f v:frame) valid &= Float.isFinite(v.x)&&Float.isFinite(v.y)&&Float.isFinite(v.z)&&Math.abs(v.length()-1)<1e-5;
    valid &= Math.abs(frame[0].dot(frame[1]))<1e-5&&Math.abs(frame[0].dot(frame[2]))<1e-5&&Math.abs(frame[1].dot(frame[2]))<1e-5;
    valid &= Float.isFinite(q.x)&&Float.isFinite(q.y)&&Float.isFinite(q.z)&&Float.isFinite(q.w);
    valid &= frame[0].dot(n)>0.99999;
    if(!valid) {failures++;System.out.println("FAIL normal="+n+" derivatives="+which);}
   }
  }
  System.out.println("failures="+failures+" / 24");
  if(args.length==0&&failures!=0)throw new AssertionError("Invalid tangent frames");
  if(args.length>0&&failures==0)throw new AssertionError("Baseline unexpectedly passed");
 }
}