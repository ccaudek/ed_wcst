# Project on cognitive flexibility in Anorexia Nervosa.

Question: Is the WCST appropriate for distinguishing between patients and controls?

## Generate Stan input

To generate the list used for Steinke Model's 7, run the following makefile:

```bash
make -f make_generate_stan_input all
```

## Run Steinke Model

To run Steinke Model 7, run the following makefile:

```bash
make -f make_fit_mod7 all
```

## Compare Groups on Model 7 Parameters

