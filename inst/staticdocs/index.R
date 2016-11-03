sd_section("Common support",
           "The common support is the set of characteristics' combinations that two groups have in common.",
           c(
             "common_support2",
             "common_support_strata2"
           )
)

sd_section("Reweighting",
           "Functions that perform the estimation of reweighting factors that balance the distribution of characteristics of one group to that of the other group.",
           c(
             "fhat_strata2",
             "reweight_strata2",
             "reweight_strata_all2",
             "reweight_strata_all4"
           )
)


sd_section("Decompositions",
           "Functions that perform the decomposition of the difference of the statistics of two groups.",
           c(
             "nopodec_mean",
             "nopodec",
             "dec_quantile",
             "dec_median",
             "dec_"
           )
)

sd_section("Observed statistics",
           "Functions that estimate observed statistics of the two groups.",
           c(
             "margin_mean",
             "margin_quantile",
             "margin_difference"
           )
)

sd_section("Data",
           "Data sets included in decr and used in examples and vignettes.",
           c(
             "invented_wages"
           )
)

sd_section("Bootstrap",
           "Bootstrap to estimate confidence intervals of the decomposition terms.",
           c(
             "boot_nopodec_mean",
             "boot_dec_quantile"
           )
)
