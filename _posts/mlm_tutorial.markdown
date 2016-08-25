---
title: "Brief intro to MLM with lme4"
output:
  html_document
---

<script type="text/x-mathjax-config">
MathJax.Hub.Config({
  TeX: { 
      equationNumbers: {
 
            autoNumber: "AMS"
      } 
  }
});
</script>

$$
\begin{equation} 
\begin{split}
\text{cort}_{ij} &=\beta_{0j}+\beta_{1j} \cdot \text{time}_{ij}+\epsilon_{ij}\\
\beta_{0j} &=\gamma_{00}+\gamma_{0[\ldots]}\cdot\text{intercept predictors}+u_{0j}\\
\beta_{1j} &=\gamma_{10}+\gamma_{1[\ldots]}\cdot\text{slope predictors}+u_{1j}
\end{split}
\label{corteq}
\end{equation} 
$$

The model in equation \\ref{corteq} says that every *i*th cortisol observation, for the *j*th participant, is predicted by an intercept ($\beta_{0j}$), and a linear lope ($\beta_{1j}$). This is equivalent to a linear model that you're used to in lm: `y ~ 1 + time`. "$\text{time}_{ij}$", in this case, is whatever you want to use -- it's probably going to be the time of day the measurement was taken for the *i*th cortisol observation, for the *j*th participant. 

You're grouping all your observations by subject, and so you can get a random effect (variation across subjects) for both the intercept and slope. That is, each of those parameters gets its own equation, which you see as the second two parts of eq \\ref{corteq}. And just as $\epsilon_{ij}$ is the deviation of every observation from the prediction, $u_{0j}$ and $u_{1j}$ is the deviation of every subject's predicted intercept and slope from the overall mean intercept and slope across all subjects. It just stands in for the fact that we're letting the slope and intercept be different for each person.

For $\beta_{0j}$ and $\beta_{1j}$ you might have multiple predictors you're interested in, so I've just indicated that as "$\gamma_{0/1[\ldots]}\cdot\text{intercept/slope predictors}$". But let's say you have just one subject level predictor for each of those subject-specific slopes and intercepts. I'll use SES as the intercept and slope predictor (though you could, in principle, use different predictors for each parameter). The three equations would be:

$$
\begin{equation} 
\begin{split}
\text{cort}_{ij} &=\beta_{0j}+\beta_{1j} \cdot \text{time}_{ij}+\epsilon_{ij}\\
\beta_{0j} &=\gamma_{00}+\gamma_{01}\cdot\text{SES}_{j}+u_{0j}\\
\beta_{1j} &=\gamma_{10}+\gamma_{11}\cdot\text{SES}_{j}+u_{1j}
\end{split}
\label{corteqfull}
\end{equation}
$$

When we use `lmer` to estimate the model, we only give it one equation. So what we need to do is substitute the equations for $\beta_{0j}$ and $\beta_{1j}$ in to get everything in term of the $\gamma$ parameters (notice that our "$\text{time}_{ij}$" variable gets multiplied through the equation for $\beta_{1j}$ and creates the interaction term $\text{SES}_{j}\times \text{time}$):

$$
\begin{equation} 
\begin{split}
\text{cort}_{ij} =& \gamma_{00}+\gamma_{01}\cdot\text{SES}_{j}+u_{0j}+\\
&(\gamma_{10}+\gamma_{11}\cdot\text{SES}_{j}+u_{1j}) \cdot \text{time}_{ij}+\epsilon_{ij}\\
\text{cort}_{ij} =& \gamma_{00}+\gamma_{01}\cdot\text{SES}_{j}+u_{0j}+\\
&\gamma_{10}\cdot \text{time}_{ij}+\gamma_{11}\cdot\text{SES}_{j}\times \text{time}_{ij}+u_{1j}\cdot \text{time}_{ij} +\epsilon_{ij}\\
\text{cort}_{ij} =& \gamma_{00}+\gamma_{01}\cdot\text{SES}_{j}+\gamma_{10}\cdot \text{time}_{ij}+\gamma_{11}\cdot\text{SES}_{j}\times \text{time}_{ij}+\\
&u_{0j}+u_{1j}\cdot \text{time}_{ij} +\epsilon_{ij}
\end{split}
\label{corteqsingle}
\end{equation}
$$

The final equation in \\ref{corteqsingle} is just reordered so that we group our fixed effects together, and our random effects and error together. Also, I'm just using "$\cdot$" and "$\times$" to set apart parameters and variable interactions a bit.

So now we have our full model equation, and we can give it to `lmer`:


```r
aModel <- lmer(cort ~ 1 + SES + time + SES:time + 
                     (1 + time | SID), 
               data=yourData)
summary(aModel)
```

It doesn't really matter that you have two days of data per person, because we just care about the time of day. You could potentially add another grouping by day, but it might make things more complicated than necessary.

When you get output from `summary`, you'll look at the term `SES:time` to see if a subject's SES predicts their cortisol slope. This is because in equation \\ref{corteqfull}, $\gamma_{11}$ is the parameter for SES predicting $\beta_{1j}$, which is your cortisol slope parameter. This parameter sticks around in the final equation \\ref{corteqsingle} for the interaction term.
