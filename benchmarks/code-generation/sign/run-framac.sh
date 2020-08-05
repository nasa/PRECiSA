#!/bin/sh

/Users/ltitolo/Desktop/frama-c-feature-bobot-float-wp/bin/frama-c \
    sign.c \
    -wp -wp-prover native:pvs \
    -wp-out . \
    -wp-pvs-lib 'interface,float@roundoff_error_props,sign_expr_cert,sign_num_cert' \
    -wp-pvs-timeout 2 \
    -wp-timeout 2 -wp-model Typed+float -wp-print # \
    
#    -wp-pvs-tactic '(then (skeep*) (beta) (flatten) (grind :exclude ("^" "mod" "expt" "L_mul_u32" "L_add_u32" "L_shr_u32") :rewrites ("add_u32_def" "mul_u32_def" "shr_u32_def" "sub_u32_def" "mod_lt_nat")))'

# -wp-pvs-lib 'interface,float@roundoff_error_props,num_cert_vwcv,expr_cert_vwcv' \
