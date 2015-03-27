---
title       : Optimization in Finance
subtitle    : Maximum Entropy Distribution of an Asset Inferrred from Option Prices
author      : Bertrand Le Nezet
job         : 
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax,bootstrap] # {mathjax, quiz, bootstrap}
mode        : standalone # {selfcontained, standalone, draft}
knit        : slidify::knit2slides
---



## Numerical implementation steps

1. Option prices snap from Google Finance API
2. Data cleaning:
    - Calculate mid from bid / ask
    - Keep only relevant option prices (put-call parity/liquidity)
3. Apply the Cross Entropy Minimization algorithm
4. Compute option prices from asset distribution
5. Convert option prices into Black-Scholes Implied Volatility

--- .class #id 
## GOOG.OQ Option quotes
 
![](google-finance-GOOG.OQ2.png)

--- .class #id 
## Put-Call parity
$d_{1}=\frac{1}{\sigma\sqrt{T-t}}\left[ln\left(\frac{S}{K}\right)+(r-\frac{\sigma^{2}}{2})(T-t)\right]$

$d_{2}=\frac{1}{\sigma\sqrt{T-t}}\left[ln\left(\frac{S}{K}\right)+(r-\frac{\sigma^{2}}{2})(T-t)\right]$

Call option: $C(S,t)=N(d_{1})S-N(d_{2})Ke^{-r(T-t)}$

Put Option: $P(S,t)=-N(-d_{1})S+N(-d_{2})Ke^{-r(T-t)}$


Pu-Call parity: $C(t)+Ke^{-r(T-t)}=P(t)+S(t)$



--- .class #id 
## Volatility smile

![](Volatility_smile_putcall2.png)

--- .class #id 
## Cross Entropy Minimization algorithm

Minimize $S(p,q)=\int_{-\infty}^{\infty}p(x)\log\left[\frac{p(x)}{q(x)}\right]dx$
    
Subject to: 
- $\int_{0}^{\infty}p(x)dx=1$
- $\forall i=1,2,..m\;\mathbb{E}\left[c_{i}(X)\right]=\int_{0}^{\infty}p(x)\; c_{i}(x)dx=ci$

1. A uniform distribution is used as a prior (non-informed prior)
2. Integrability constraint
2. The option prices used as constraints should be linearly independant
3. Forward is also used as a constraint
4. Choice of asset step for the discretisation / boundaries
5. The Lagrangian function is minimized using an optimization routine (Limited-memory Broyden-Fletcher-Goldfarb-Shanno)



--- .class #id 
## GOOG.OQ Asset distribution (1m/2m/3m/6m)

![](asset-dist-multi-expiry.png)

--- .class #id 
## Asset distribution with constant volatility (25%)

![](constant_vol_asset_dist.png)

--- .class #id 
## GOOG.OQ Volatility smile (from Asset distribution)

$c(K)=D(T)\int_{0}^{\infty}p(x)\;\left(x-K\right)^{+}dx$ 

$p(K)=D(T)\int_{0}^{\infty}p(x)\;\left(K-x\right)^{+}dx$
![](implied-volatility-surface.png)

--- .class #id 
## Volatility smile (from Quotes)

![](GOOG.OQ-implied-volatility-surface.png)

--- .class #id 
## Conclusion

1. We have observed that assets distribution inferred from options prices are not log-normal (i.e. Volatility is not constant)  
2. Principle of Minimum Cross-Entropy can be used to estimate the distribution of an asset without any assumptions (non-parametric approach)
3. The algorithm is stable and fast
4. This method can be used to perform implied volatility interpolation/extrapolation from only few quotes

--- .class #id 
## Appendix

--- .class #id 
## Kullback-Leibler Relative entropy
1. In Information Theory, Shannon defines the entropy as a measure of unpredictability of information content
2. the KL relative entropy (or KL divergence) is a non-suymetric mesure of the difference between two probability distribution P and Q. 
3. $D_{KL}(P\Vert Q)=\int_{-\infty}^{\infty}p(x)\ln\frac{p(x)}{q(x)}dx$
4. Properties:
- KL is equal to zero if P and Q are identical
- KL relative entropy is always positive

--- .class #id 
## Calculation of Relative Entropy

<img src="assets/fig/unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />

--- .class #id 
## Formulating the Optimization problem

We have one prior probability density function $q(x)$

We have $m$ price constraints: $\forall i=1,2,..m\; d_{i}=D(T)\mathbb{E}_{\mathbb{Q}}\left[c_{i}(X_{T})\right]$
where
- $D(T)=e^{-(r-q)T}$ represents the discount factor
- $c_{i}(X_{t})$ denotes the $i$th option pay-off function at expiry
depenent only on the asset value at expiry
- $d_{i}$is the corresponding option price
- $r$ risk-free rate for $T$
- $q$ dividend yield

--- .class #id 
## Formulating the Optimization problem (2)

Minimize $S(p,q)=\int_{-\infty}^{\infty}p(x)\log\left[\frac{p(x)}{q(x)}\right]dx$

Subject to 2 constraints:

1. $\int_{0}^{\infty}p(x)dx=1$
2. $\forall i=1,2,..m\;\mathbb{E}\left[c_{i}(X)\right]=\int_{0}^{\infty}p(x)\; c_{i}(x)dx=ci$

This is a standard constrained optimization problem which solved by using the method of Lagrange which transforms a problem in n variable and m constraints into an unconstrainted optimization with n+m variables.

--- .class #id 
## Objective function

$H(p)=-\int_{0}^{\infty}p(x)\log\left[\frac{p(x)}{q(x)}\right]dx+(1+\lambda_{0})\int_{0}^{\infty}p(x)dx+\sum_{i=1}^{m}\lambda_{i}\int_{0}^{\infty}p(x)\; c_{i}(x)dx$

From standard calculus, we know that the minimum $\lambda^{*}=(\lambda_{0}^{*},\ldots,\lambda_{M}^{*})$
is reached when:
- the gradient (vector of derivatives)$\delta H$ is equal to zero:
$\delta H(\lambda^{*})=\int_{0}^{\infty}\left[-\log\left[\frac{p(x)}{q(x)}\right]+\lambda_{0}+\sum_{i=1}^{m}\lambda_{i}c_{i}(x)\right]\delta p(x)dx=0$
(necessary condition)
- the hessian (matrix of second derivatives) is positive definite (sufficient
condition)

--- .class #id 
## Objective function solution

This leads immediately to the following explicit representation of
the MED:

$p(x)=\frac{q(x)}{\mu}\exp\left(\sum_{i=1}^{m}\lambda_{i}c_{i}(x)\right)$,
$\mu=\int_{0}^{\infty}q(x)\exp\left(\sum_{i=1}^{m}\lambda_{i}c_{i}(x)\right)dx$

--- .class #id 
## Impact of constraints on MXED

![](impacts_constraints.png)

--- .class #id 
## End

--- .class #id 

