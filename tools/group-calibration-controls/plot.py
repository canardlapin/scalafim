"""Standalone scientific figures; all plotted values come from retained counts."""
import os,pathlib,json
P=pathlib.Path(__file__).resolve().parent
os.environ.setdefault('MPLCONFIGDIR',str(P/'mpl-config'))
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import numpy as np
S=json.loads((P/'summary.json').read_text())
plt.rcParams.update({'font.family':'DejaVu Sans','font.size':10,'axes.labelcolor':'#314050','text.color':'#203040','axes.edgecolor':'#d5dce4','axes.spines.top':False,'axes.spines.right':False,'axes.titleweight':'bold','axes.titlepad':14,'savefig.facecolor':'#ffffff'})
blue='#247391';orange='#c45635';green='#377954';purple='#7553a1';gray='#5c6876'

def pick(stage,**kw):
    return next(r for r in S[stage]['rows'] if all(r['cell'][k]==v for k,v in kw.items()))

def point(ax,rec,m,y,col,marker='o',offset=0):
    d=rec['methods'][m];v=100*d['nullRate'];lo,hi=np.array(d['nullCI95'])*100
    ax.errorbar(v,y+offset,xerr=[[v-lo],[hi-v]],fmt=marker,color=col,ms=5,capsize=2,lw=1.15)

fig,axs=plt.subplots(1,2,figsize=(14.5,6.4),gridspec_kw={'width_ratios':[1.15,1]})
fig.subplots_adjust(top=.76,bottom=.20,left=.19,right=.97,wspace=.38)
fig.text(.035,.94,'Calibration improves in Gaussian cases; skewness remains unresolved',size=18,weight='bold')
fig.text(.035,.887,'Fresh simulations · scalar group contrast with a covariate · 10,000 studies per setting',size=11,color=gray)
a,b=axs
methods=['HC2_Satt','HC3_Satt','wild_HC2_rad','wild_HC2_mammen','wild_HC3_rad','wild_HC3_mammen']
labels=['HC2 / Satterthwaite','HC3 / Satterthwaite','Wild HC2 · Rademacher','Wild HC2 · Mammen','Wild HC3 · Rademacher','Wild HC3 · Mammen']
normal=pick('confirmation',n=20,variance='reverse',error='normal',tau2=0.)
skew=pick('confirmation',n=20,variance='reverse',error='skew',tau2=0.)
for i,m in enumerate(methods):
    point(a,normal,m,i,blue,offset=-.14);point(a,skew,m,i,orange,marker='s',offset=.14)
a.set_yticks(range(len(labels)),labels);a.invert_yaxis();a.set_xlim(0,15);a.set_xticks([0,5,10,15]);a.axvline(5,color=gray,ls='--',lw=1);a.grid(axis='x',alpha=.15)
a.set_xlabel('False positives (%)');a.set_title('A  Error shape changes the conclusion',loc='left',size=12,y=1.10,pad=8)
a.legend(handles=[Line2D([],[],marker='o',color=blue,label='Gaussian'),Line2D([],[],marker='s',color=orange,label='Skewed')],loc='upper left',bbox_to_anchor=(0,-.17),ncol=2,frameon=False)
a.text(0,1.045,'20 participants · 5 versus 15 · reversed variances',transform=a.transAxes,size=9,color=gray)
for m,label,col in [('native_PM_mKH','Native PM/mKH',orange),('plugin_GLS_t','Plug-in WLS t',purple),('plugin_HC3_Satt','WLS HC3 / Satterthwaite',blue),('oracle_WLS_t','True-variance WLS t',green)]:
    ds=[pick('estimated',n=80,variance='reverse',tau2=0.,firstLevelDf=df)['methods'][m] for df in [0,40,8]]
    v=np.array([d['nullRate'] for d in ds])*100;ci=np.array([d['nullCI95'] for d in ds])*100
    b.errorbar(range(3),v,yerr=np.array([v-ci[:,0],ci[:,1]-v]),fmt={'native_PM_mKH':'o-','plugin_GLS_t':'s--','plugin_HC3_Satt':'D-.','oracle_WLS_t':'^:'}[m],color=col,label=label,ms=5,capsize=2,lw=1.5)
b.set_xticks([0,1,2],['Known variance','40 df','8 df']);b.set_ylim(0,12);b.set_yticks([0,2.5,5,7.5,10]);b.axhline(5,color=gray,ls='--',lw=1);b.grid(axis='y',alpha=.15)
b.set_ylabel('False positives (%)');b.set_xlabel('First-level variance information');b.set_title('B  Noisy first-level SEs inflate PM/mKH',loc='left',size=12,y=1.10,pad=8)
b.text(0,1.045,'80 participants · 20 versus 60 · Gaussian · τ² = 0',transform=b.transAxes,size=9,color=gray)
b.legend(frameon=False,fontsize=9,loc='upper left',bbox_to_anchor=(0,-.17),ncol=2,columnspacing=1)
fig.text(.035,.025,'Dashed line: nominal 5%. Bars: pointwise 95% binomial intervals. Truth-informed controls are diagnostic only; no new default is admitted.',size=9,color=gray)
fig.savefig(P/'calibration-overview.png',dpi=170);fig.savefig(P/'calibration-overview.svg');plt.close(fig)

fig,axs=plt.subplots(2,2,figsize=(14.5,11.5),sharex='col',sharey=True)
fig.subplots_adjust(top=.83,bottom=.12,left=.20,right=.965,wspace=.22,hspace=.28)
fig.text(.035,.955,'Feasible precision weighting: Gaussian evidence and the cost of noisy weights',size=17,weight='bold')
fig.text(.035,.915,'Fresh follow-up · weights use estimated first-level variance only · no true or estimated τ² in the candidate',size=11,color=gray)
fig.legend(handles=[Line2D([],[],marker='o',color=blue,label='Inverse-variance HC3 / Satterthwaite'),Line2D([],[],marker='s',color=orange,label='Equal-subject HC3 / Satterthwaite')],loc='upper left',bbox_to_anchor=(.19,.897),ncol=2,frameon=False,fontsize=10)
cases=[(v,t,df) for v in ['spread','reverse'] for t in [0.,.2] for df in [0,40,8]]
labels=[f'{v.capitalize()} · τ² {t:g} · '+('known v' if df==0 else f'{df} df') for v,t,df in cases]
for row,n in enumerate([20,80]):
    for i,(v,t,df) in enumerate(cases):
        r=pick('feasible-weighting',n=n,variance=v,tau2=t,firstLevelDf=df)
        for m,col,marker,off in [('fixed_vhat_HC3_Satt',blue,'o',-.13),('unweighted_HC3_Satt',orange,'s',.13)]:
            point(axs[row,0],r,m,i,col,marker,off)
            d=r['methods'][m];power=100*d['powerAtNominalAlpha'];lo,hi=np.array(d['powerCI95'])*100
            axs[row,1].errorbar(power,i+off,xerr=[[power-lo],[hi-power]],fmt=marker,color=col,ms=4.5,capsize=2,lw=1.0)
    for col in [0,1]:
        a=axs[row,col];a.set_yticks(range(12),labels);a.set_ylim(11.6,-.6);a.grid(axis='x',alpha=.15)
        for j in [2.5,5.5,8.5]:a.axhline(j,color='#e5e9ef',lw=.8)
        a.set_title(f'{n} participants · '+('False positives' if col==0 else 'Power at effect +0.7'),loc='left',size=12)
    axs[row,0].axvline(5,color=gray,ls='--',lw=1);axs[row,0].set_xlim(0,6);axs[row,0].set_xticks(range(7))
    axs[row,1].set_xlim(0,100);axs[row,1].set_xticks(range(0,101,20))
axs[1,0].set_xlabel('Rejection under the null (%)');axs[1,1].set_xlabel('Rejection at a fixed nonzero effect (%)')
fig.text(.035,.050,'24 Gaussian settings, 10,000 studies each. Bars: pointwise 95% binomial intervals. Power uses the implemented nominal 5% threshold.',size=9,color=gray)
fig.text(.035,.028,'These are conditional-model tests of the same group coefficient. General non-Gaussian, repeated-subject, F and spatial inference remain unqualified.',size=9,color=gray)
fig.savefig(P/'feasible-weighting.png',dpi=170);fig.savefig(P/'feasible-weighting.svg');plt.close(fig)
