"""Independent algebra audit of the shared condition/trial HRF architecture.

Uses dense matrices ONLY as small numerical test oracles. This is not a
production solver, a real-data validation, or a runtime benchmark.
Adapted from the user-supplied hrf_alignment_review.zip/audit.py.
The original identities and assertions are retained; normalization, feature
queries and compact condition checks are added below.
Requires NumPy and SciPy. Run from the repository root:
  OPENBLAS_NUM_THREADS=1 python3 tools/validation/profile_hrf_unification.py
"""
from __future__ import annotations
import json
from pathlib import Path
import numpy as np
from scipy.linalg import cho_factor, cho_solve, solve_triangular


def solve(a, b):
    return cho_solve(cho_factor(a, lower=True), b)


def rel(a, b):
    return float(np.linalg.norm(a-b)/max(np.linalg.norm(b), 1e-15))


def logdet(a):
    return float(2*np.log(np.diag(cho_factor(a, lower=True)[0])).sum())


def fd(fun, theta, step=1e-4):
    v=fun(theta); d=len(theta)
    g=np.empty((d,)+v.shape); h=np.empty((d,d)+v.shape)
    eye=np.eye(d)*step
    for p in range(d):
        vp=fun(theta+eye[p]); vm=fun(theta-eye[p])
        g[p]=(vp-vm)/(2*step)
        h[p,p]=(vp-2*v+vm)/step**2
        for q in range(p):
            h[p,q]=h[q,p]=(fun(theta+eye[p]+eye[q])-fun(theta+eye[p]-eye[q])
                -fun(theta-eye[p]+eye[q])+fun(theta-eye[p]-eye[q]))/(4*step**2)
    return v,g,h


def energy_jet(s, b, G):
    """Reusable second-order reduction for E = s - b' inv(G) b."""
    sv,sp,spp=s; bv,bp,bpp=b; gv,gp,gpp=G
    a=solve(gv,bv); d=len(sp)
    u=np.array([bp[p]-gp[p]@a for p in range(d)])
    au=np.array([solve(gv,u[p]) for p in range(d)])
    value=sv-bv@a
    grad=np.array([sp[p]-2*bp[p]@a+a@gp[p]@a for p in range(d)])
    hess=np.array([[spp[p,q]-2*bpp[p,q]@a+a@gpp[p,q]@a-2*u[p]@au[q]
                    for q in range(d)] for p in range(d)])
    return value,grad,hess


def run():
    rng=np.random.default_rng(1061)
    T,N,C,V=84,18,3,7
    onsets=np.sort(rng.choice(np.arange(2,66),N,replace=False))
    lag=np.arange(T)[:,None]-onsets[None,:]
    M=np.eye(C)[np.arange(N)%C]
    Mn=M/np.sqrt(M.sum(0))[None,:]
    P=np.eye(N)-Mn@Mn.T
    F=np.column_stack([np.ones(T),np.linspace(-1,1,T),np.sin(np.arange(T)/13)])
    pF=F.shape[1]
    Q=np.linalg.qr(F,mode='complete')[0]; Qf=Q[:,:pF]; Qperp=Q[:,pF:]
    theta=np.array([4.7,np.log(1.7)])
    alpha=.8

    def Xjet(th):
        tau,eta=th; sig=np.exp(eta); u=lag-tau; s2=sig**2
        X=np.exp(-.5*u*u/s2)*(lag>=0)
        xp=np.array([X*u/s2,X*u*u/s2])
        xpp=np.empty((2,2,T,N))
        xpp[0,0]=X*(u*u/s2**2-1/s2)
        xpp[0,1]=xpp[1,0]=X*(u**3/s2**2-2*u/s2)
        xpp[1,1]=X*(u**4/s2**2-2*u*u/s2)
        return X,xp,xpp

    X=Xjet(theta)[0]
    Y=X@rng.normal(size=(N,V))+F@rng.normal(size=(pF,V))+.3*rng.normal(size=(T,V))

    def master(th, a_scale, data=Y):
        x=Xjet(th)[0]; z=np.column_stack([F,x@M])
        if a_scale==0:
            coef=solve(z.T@z,z.T@data)
            q=data-z@coef
            amp=M@coef[pF:]
            return coef,amp,q,np.sum(data*q,axis=0)
        S=np.eye(T)+a_scale*x@x.T
        rz=solve(S,z); ry=solve(S,data)
        coef=solve(z.T@rz,z.T@ry)
        q=ry-rz@coef
        amp=M@coef[pF:]+a_scale*x.T@q
        return coef,amp,q,np.sum(data*q,axis=0)

    max_amp=max_energy=max_deviation=0.
    for a_scale in [.03,.8,8.]:
        co,amp,q,E=master(theta,a_scale)
        D=np.column_stack([F,X]); penalty=np.zeros((pF+N,pF+N));penalty[pF:,pF:]=P/a_scale
        w=solve(D.T@D+penalty,D.T@Y)
        residual=Y-D@w
        Ed=np.sum(residual**2,axis=0)+np.sum(w*(penalty@w),axis=0)
        max_amp=max(max_amp,rel(amp,w[pF:]))
        max_energy=max(max_energy,rel(E,Ed))
        max_deviation=max(max_deviation,float(np.max(np.abs(M.T@(amp-M@co[pF:])))))
    z0=np.column_stack([F,X@M]); cf0=np.linalg.lstsq(z0,Y,rcond=None)[0]
    condition_error=rel(master(theta,0)[1],M@cf0[pF:])

    # Correct versus nuisance-residualized determinant: difference is not constant.
    det_rows=[];det_identity=0.; small_det_identity=0.
    for tau in [3.3,4.7,6.2]:
        th=theta.copy(); th[0]=tau; x=Xjet(th)[0]
        K=np.eye(T)+alpha*x@P@x.T
        S=np.eye(T)+alpha*x@x.T
        ld=logdet(K); reduced=logdet(Qperp.T@K@Qperp)
        correction=logdet(Qf.T@solve(K,Qf))
        det_identity=max(det_identity,abs(reduced-ld-correction))
        U=np.sqrt(alpha)*x@Mn
        corrected_ld=logdet(S)+logdet(np.eye(C)-U.T@solve(S,U))
        small_det_identity=max(small_det_identity,abs(corrected_ld-ld))
        det_rows.append({'tau':tau,'full_logdet':ld,'nuisance_contrast_logdet':reduced,
                         'difference':reduced-ld})

    # Exact normal-equation jet, with all nuisance coefficients included.
    x,xp,xpp=Xjet(theta)
    D=np.column_stack([F,x]); dp=np.zeros((2,T,pF+N)); dp[:,:,pF:]=xp
    dpp=np.zeros((2,2,T,pF+N)); dpp[:,:,:,pF:]=xpp
    penalty=np.zeros((pF+N,pF+N));penalty[pF:,pF:]=P/alpha
    G=D.T@D+penalty
    gp=np.array([dp[p].T@D+D.T@dp[p] for p in range(2)])
    gpp=np.array([[dpp[p,q].T@D+dp[p].T@dp[q]+dp[q].T@dp[p]+D.T@dpp[p,q]
                  for q in range(2)] for p in range(2)])
    y=Y[:,0]
    b=(D.T@y,np.array([d.T@y for d in dp]),np.array([[d.T@y for d in row] for row in dpp]))
    jet=energy_jet((y@y,np.zeros(2),np.zeros((2,2))),b,(G,gp,gpp))
    ej=fd(lambda th:np.array(master(th,alpha,y[:,None])[3][0]),theta)

    # Same scalar jet reducer, now with a time-whitened C+nuisance-sized system.
    def small_summaries(th):
        x=Xjet(th)[0]; z=np.column_stack([F,x@M]); S=np.eye(T)+alpha*x@x.T
        L=np.linalg.cholesky(S)
        v=solve_triangular(L,y,lower=True)
        B=solve_triangular(L,z,lower=True)
        return np.array(v@v),B.T@v,B.T@B
    sj=fd(lambda th:small_summaries(th)[0],theta)
    bj=fd(lambda th:small_summaries(th)[1],theta)
    hj=fd(lambda th:small_summaries(th)[2],theta)
    sjout=energy_jet(sj,bj,hj)

    # Conditional readout Taylor operators and a beta-free pattern operator.
    L0=solve(G,D.T)
    Lp=np.array([solve(G,dp[p].T-gp[p]@L0) for p in range(2)])
    Lpp=np.array([[solve(G,dpp[p,q].T-gpp[p,q]@L0-gp[p]@Lp[q]-gp[q]@Lp[p])
                    for q in range(2)] for p in range(2)])
    operators=[L0[pF:],Lp[0,pF:],Lp[1,pF:],.5*Lpp[0,0,pF:],Lpp[0,1,pF:],.5*Lpp[1,1,pF:]]
    delta=rng.uniform(-1,1,size=(2,V))*np.array([[.09],[.02]])
    weights=np.array([np.ones(V),delta[0],delta[1],delta[0]**2,delta[0]*delta[1],delta[1]**2])
    beta=sum((op@Y)*w[None,:] for op,w in zip(operators,weights))
    vx=rng.normal(size=V); ts=rng.normal(size=N)
    forward=sum(op@(Y@(w*vx)) for op,w in zip(operators,weights))
    adjoint=sum(w*(Y.T@(op.T@ts)) for op,w in zip(operators,weights))
    taylor_errors=[]
    for scale in [1.,.5,.25]:
        dd=delta*scale
        ww=[np.ones(V),dd[0],dd[1],dd[0]**2,dd[0]*dd[1],dd[1]**2]
        approx=sum((op@Y)*w[None,:] for op,w in zip(operators,ww))
        exact=np.column_stack([master(theta+dd[:,v],alpha,Y[:,v:v+1])[1][:,0] for v in range(V)])
        taylor_errors.append(rel(approx,exact))

    # Duplicate onsets: sufficient schedule for covariance is sum of squared gains.
    tt=np.array([3,3,11,23,23,23,51]); gains=rng.uniform(.5,1.7,len(tt))
    H=np.zeros((T,T)); kernel=np.exp(-.5*((np.arange(T)-4.7)/1.7)**2)
    for t in range(T): H[t:,t]=kernel[:T-t]
    E=np.zeros((T,len(tt))); E[tt,np.arange(len(tt))]=gains
    counts=np.bincount(tt,weights=gains*gains,minlength=T)
    covariance_event_error=rel((H@E)@(H@E).T,(H*counts[None,:])@H.T)

    results={
      'scope':'Independent dense algebra checks, not production kernels or empirical timing benchmarks.',
      'shape':{'timepoints':T,'trials':N,'conditions':C,'nuisance_columns':pF,'voxels':V},
      'master_amplitude_vs_original_penalty_max_relative_error':max_amp,
      'master_penalized_energy_vs_original_max_relative_error':max_energy,
      'within_condition_trial_deviation_sum_max_absolute':max_deviation,
      'alpha_zero_condition_only_relative_error':condition_error,
      'full_vs_nuisance_determinant_identity_max_abs_error':det_identity,
      'small_condition_logdet_correction_max_abs_error':small_det_identity,
      'determinant_counterexample':det_rows,
      'determinant_difference_variation_across_shapes':float(np.ptp([r['difference'] for r in det_rows])),
      'normal_energy_jet_gradient_vs_fd_relative_error':rel(jet[1],ej[1]),
      'normal_energy_jet_hessian_vs_fd_relative_error':rel(jet[2],ej[2]),
      'small_whitened_energy_jet_gradient_vs_normal_relative_error':rel(sjout[1],jet[1]),
      'small_whitened_energy_jet_hessian_vs_normal_relative_error':rel(sjout[2],jet[2]),
      'polynomial_pattern_forward_relative_error':rel(forward,beta@vx),
      'polynomial_pattern_transpose_relative_error':rel(adjoint,beta.T@ts),
      'conditional_readout_second_order_relative_errors_at_scales_1_half_quarter':taylor_errors,
      'convergence_error_ratios':list(np.array(taylor_errors[:-1])/np.array(taylor_errors[1:])),
      'impulse_schedule_covariance_aggregation_relative_error':covariance_event_error,
      'limitations':[
        'One small deterministic experiment does not establish HRF recovery under realistic noise.',
        'Profile derivatives and conditional readouts are checked locally, not global cell routing.',
        'The whitened backend in this audit is a dense oracle; short-state checks are rerun separately.',
        'Polynomial operator parity is exact for the Taylor approximation, not for globally adaptive fitting.',
        'No production Scala code was changed and no hardware throughput claim is made.'
      ]}
    assert max_amp<1e-10 and max_energy<1e-10 and condition_error<1e-10
    assert det_identity<1e-10 and small_det_identity<1e-10
    assert results['determinant_difference_variation_across_shapes']>1e-4
    assert rel(jet[2],ej[2])<2e-5 and rel(sjout[2],jet[2])<2e-5
    assert rel(forward,beta@vx)<1e-11 and rel(adjoint,beta.T@ts)<1e-11
    assert min(results['convergence_error_ratios'])>6
    return results


def compact_and_penalty_checks():
    """Independent least-squares oracles for the added condition/query contracts."""
    from scipy.linalg import block_diag, null_space
    rng = np.random.default_rng(1948)
    T, N, C, features, queries = 53, 15, 3, 4, 5
    F = np.column_stack([np.ones(T), np.linspace(-1, 1, T)])
    M = np.eye(C)[np.arange(N) % C]
    P = np.eye(N) - M @ np.linalg.solve(M.T @ M, M.T)
    Phi = P @ rng.normal(size=(N, features))
    Gamma = np.diag([0.3, 0.6, 1.2, 1.7])
    lam = 0.8
    X = rng.normal(size=(T, N))
    y = rng.normal(size=T)
    query = rng.normal(size=(queries, N))
    contrasts = null_space(M.T)

    def feature_fit(scale, scale_gamma=True):
        x, penalty = scale * X, scale**2 * lam
        gamma = scale**2 * Gamma if scale_gamma else Gamma
        Z = np.column_stack([F, x @ M, x @ Phi])
        D = block_diag(np.zeros((2 + C, 2 + C)), gamma)
        S = np.eye(T) + x @ x.T / penalty
        RZ, Ry = solve(S, Z), solve(S, y)
        c = solve(Z.T @ RZ + D, Z.T @ Ry)
        q = Ry - RZ @ c
        u = x.T @ q / penalty
        a = M @ c[2:2+C] + Phi @ c[2+C:] + u
        energy = q @ q + penalty * (u @ u) + c @ D @ c
        probes = query @ M @ c[2:2+C] + query @ Phi @ c[2+C:] + (x @ query.T).T @ q / penalty
        # Augmented QR/SVD least squares, independent of the release equations.
        direct_design = np.column_stack([Z, x @ contrasts])
        root = block_diag(np.zeros((2 + C, 2 + C)), np.linalg.cholesky(gamma).T,
                          np.sqrt(penalty) * np.eye(N - C))
        coef = np.linalg.lstsq(np.vstack([direct_design, root]),
                              np.r_[y, np.zeros(root.shape[0])], rcond=None)[0]
        direct_a = M @ coef[2:2+C] + Phi @ coef[2+C:2+C+features] + contrasts @ coef[2+C+features:]
        return energy, a, probes, direct_a, u, c[2+C:]

    energy, a, probes, direct_a, u, w = feature_fit(1.0)
    scaled_energy, scaled_a, *_ = feature_fit(2.3)
    wrong_energy, *_ = feature_fit(2.3, scale_gamma=False)

    # A polynomial observed condition family with an exactly representable union.
    Qf = np.linalg.qr(F, mode='reduced')[0]
    blocks = [rng.normal(size=(T, C)) for _ in range(3)]
    blocks = [b - Qf @ (Qf.T @ b) for b in blocks]
    U = np.linalg.qr(np.column_stack(blocks), mode='reduced')[0]
    yr = y - Qf @ (Qf.T @ y)
    z = U.T @ yr
    compact_errors = []
    nuisance_errors = []
    for theta in [-0.4, 0.0, 0.3]:
        A = blocks[0] + theta * blocks[1] + theta**2 * blocks[2]
        D = U.T @ A
        beta = np.linalg.lstsq(D, z, rcond=None)[0]
        raw_A = A + F @ np.ones((2, C))
        gamma = np.linalg.lstsq(F, y - raw_A @ beta, rcond=None)[0]
        direct = np.linalg.lstsq(np.column_stack([F, raw_A]), y, rcond=None)[0]
        compact_energy = yr @ yr - np.linalg.norm(np.linalg.qr(D, mode='reduced')[0].T @ z)**2
        direct_energy = np.linalg.norm(y - F @ direct[:2] - raw_A @ direct[2:])**2
        compact_errors.append(abs(compact_energy - direct_energy))
        nuisance_errors.append(np.max(np.abs(gamma - direct[:2])))

    result = {
        'feature_release_vs_augmented_lstsq_amplitude_max_abs': float(np.max(np.abs(a - direct_a))),
        'direct_query_vs_augmented_lstsq_max_abs': float(np.max(np.abs(probes - query @ direct_a))),
        'feature_centering_max_abs': float(np.max(np.abs(M.T @ u))),
        'feature_stationarity_max_abs': float(np.max(np.abs(lam * Phi.T @ u - Gamma @ w))),
        'feature_residual_projection_norm_not_zero': float(np.linalg.norm(Phi.T @ u)),
        'joint_lambda_gamma_scale_energy_abs': float(abs(energy - scaled_energy)),
        'joint_lambda_gamma_scale_amplitude_max_abs': float(np.max(np.abs(a - 2.3 * scaled_a))),
        'lambda_only_scale_energy_discrepancy': float(abs(energy - wrong_energy)),
        'compact_condition_profile_energy_max_abs': float(max(compact_errors)),
        'compact_condition_nuisance_recovery_max_abs': float(max(nuisance_errors)),
        'compact_rank': U.shape[1],
        'basis_penalty_scalar_original_forward_inverse': [1.0, 16.0, 1.0],
    }
    for name, value in result.items():
        if name.endswith(('_max_abs', '_energy_abs')):
            assert value < 1e-10, (name, value)
    assert result['lambda_only_scale_energy_discrepancy'] > 1e-5
    assert result['feature_residual_projection_norm_not_zero'] > 1e-4
    return result


if __name__=='__main__':
    import hashlib
    import platform
    import scipy
    result = {
        'runtime': {'python': platform.python_version(), 'numpy': np.__version__, 'scipy': scipy.__version__},
        'script_sha256': hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        'alignment': run(),
        'additional_contracts': compact_and_penalty_checks(),
    }
    print(json.dumps(result,indent=2))
