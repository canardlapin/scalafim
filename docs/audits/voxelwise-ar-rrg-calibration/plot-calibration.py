#!/usr/bin/env python3
"""Render separate fresh coordinate coverage estimates with simultaneous limits."""
import json,sys
from pathlib import Path
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
summary=json.loads(Path(sys.argv[1]).read_text())
gate=summary['qualification']; coordinates=gate['coordinates']
labels=summary['coordinate_order']; scenarios=['baseline','strong_ar','multiple_runs','censor_continuous']
fig,axes=plt.subplots(2,2,figsize=(12,9.2),sharex=True,sharey=True)
for ax,scenario in zip(axes.flat,scenarios):
 rows=[r for r in coordinates if r['scenario']==scenario]
 ax.axvspan(.92,.98,color='#dbeafe',alpha=.8)
 ax.axvline(.95,color='#64748b',linestyle=':',linewidth=1)
 for i,r in enumerate(rows):
  lo,hi=r['simultaneous_interval'];x=r['coverage'];color='#b91c1c' if r['outside_band'] else '#1d4ed8'
  ax.errorbar(x,i,xerr=[[x-lo],[hi-x]],fmt='o',color=color,capsize=2,markersize=4,linewidth=1)
 ax.set_title(scenario.replace('_',' ').title()+f"  (N={gate['checkpoint_per_scenario']})",loc='left',fontsize=12,weight='bold')
 ax.set_yticks(range(12),[s.replace('coefficient_','b').replace('_voxel_',', v') for s in labels])
 ax.tick_params(axis='y',labelleft=True,labelsize=9)
 ax.grid(axis='x',color='#e2e8f0',linewidth=.6)
 for side in ('top','right'):ax.spines[side].set_visible(False)
 ax.set_xlabel('Coverage of requested 95% intervals')
axes[0,0].invert_yaxis()
minimum=min(r['simultaneous_interval'][0] for r in coordinates)
axes[0,0].set_xlim(max(0,minimum-.025),1.005)
fig.suptitle('Fresh qualification of fitted joint AR bootstrap',x=.06,ha='left',fontsize=17,weight='bold')
fig.text(.06,.026,'Bars: exact simultaneous binomial limits, adjusted across 48 coordinates and 3 planned looks.\nShading: prespecified 92–98% band. Red: the entire confidence interval lies outside that band.',fontsize=10,color='#334155')
fig.tight_layout(rect=(0,.075,1,.94),h_pad=2.5,w_pad=2.5)
output=Path(sys.argv[2]); fig.savefig(output.with_suffix('.svg'));fig.savefig(output.with_suffix('.png'),dpi=160)

svg = output.with_suffix(".svg")
svg.write_text("\n".join(line.rstrip() for line in svg.read_text().splitlines()) + "\n")
