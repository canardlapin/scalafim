"""Scientific qualification figure; no application UI or inference-eligibility claim."""
import os
os.environ.setdefault('MPLCONFIGDIR','/private/tmp/scalafim-group-nuisance-mpl')
import pathlib,json,numpy as np,matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from scipy.stats import beta
P=pathlib.Path(__file__).resolve().parent
rows=[json.loads(l) for l in (P/'confirmation.jsonl').read_text().splitlines()]
native=json.loads((P/'native-review-20-0.json').read_text())
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':10,'axes.spines.top':False,'axes.spines.right':False,'axes.edgecolor':'#b8bec7','text.color':'#202b3b','axes.labelcolor':'#202b3b','xtick.color':'#425167','ytick.color':'#425167'})
fig=plt.figure(figsize=(13.5,9),facecolor='#fafbf9');gs=fig.add_gridspec(2,2,width_ratios=[1.25,1],hspace=.54,wspace=.45,left=.25,right=.97,top=.83,bottom=.13)
ax=fig.add_subplot(gs[:,0]);colors={'CR2':'#277f9b','WCR2':'#b65c49'}
labels=[]
for i,r in enumerate(rows):
 labels.append(f"n={r['n']} · {'1:1' if r['group']=='balanced' else '1:3'} groups\n{r['error']} · {r['variance']} · τ²={r['tau2']:g}")
 for m,off in [('CR2',-.13),('WCR2',.13)]:
  k=r['null'][m];n=r['trials'];p=k/n;lo=beta.ppf(.025,k,n-k+1);hi=beta.ppf(.975,k+1,n-k)
  ax.errorbar(100*p,i+off,xerr=[[100*(p-lo)],[100*(hi-p)]],fmt='o' if m=='CR2' else 's',ms=5,c=colors[m],lw=1.5,capsize=2,label=m if i==0 else None)
ax.axvline(5,color='#667569',ls='--',lw=1.3,zorder=0);ax.set_yticks(range(len(rows)),labels);ax.invert_yaxis();ax.set_xlim(2.5,13);ax.set_xticks([3,5,7,9,11,13]);ax.set_xlabel('False positives (%) · 95% binomial intervals');ax.grid(axis='x',color='#dfe5e8',lw=.6);ax.set_title('Fresh confirmation',loc='left',fontweight='bold',pad=18);ax.legend(loc='lower right',frameon=False)
# Replace terse research labels with definitions in the footer.
ax2=fig.add_subplot(gs[0,1]);r=native['rows'];x=np.arange(1,len(r)+1);shares=np.array([z['share'] for z in r])*100
ax2.bar(x,shares,color=['#277f9b' if z['influence']>0 else '#b65c49' for z in r],width=.72);[ax2.text(i+1,v+.15,'+' if r[i]['influence']>0 else '−',ha='center',fontsize=7) for i,v in enumerate(shares)];ax2.set_ylim(0,max(shares)*1.15);ax2.set_xticks([1,5,10,15,20]);ax2.set_xlabel('Participant');ax2.set_ylabel('Working variance share (%)');ax2.set_title('Where this contrast gets information',loc='left',fontweight='bold',pad=18);ax2.grid(axis='y',color='#dfe5e8',lw=.6);ax2.set_axisbelow(True)
ax3=fig.add_subplot(gs[1,1]);ax3.plot(x,[z['leverage'] for z in r],'o-',color='#526781',lw=1,ms=4);ax3.set_xticks([1,5,10,15,20]);ax3.set_xlabel('Participant');ax3.set_ylabel('Design leverage');ax3.set_ylim(0,.31);ax3.grid(axis='y',color='#dfe5e8',lw=.6);ax3.set_title('20 participants · 17 residual df\n7.55 working CR2 df for the group contrast',loc='left',fontweight='bold',pad=17,fontsize=10)
fig.text(.045,.945,'Nuisance-adjusted inference still needs qualification',fontsize=21,fontweight='bold');fig.text(.045,.900,'Neither candidate earns a general default. Design diagnostics explain concentration; they do not certify inference.',fontsize=11,color='#526173')
fig.text(.045,.054,'CR2: identity-target HC2 with Satterthwaite t.  WCR2: null-restricted Rademacher bootstrap, HC2 residuals and studentization.\n20,000 independent studies per confirmation cell; 1,999 independent draws per study. Nonzero intercept and nuisance slope.\nRight: native ScalaFIM review of an unweighted 5-versus-15 contrast with a covariate. Shares assume identity covariance; signs above bars show contrast direction.',fontsize=8.6,color='#526173',linespacing=1.6)
fig.savefig(P/'qualification.png',dpi=160,facecolor=fig.get_facecolor());plt.close(fig)
