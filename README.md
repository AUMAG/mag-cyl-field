# Magnetic field calculations of cylindrical arc magnets and coils
This page features a Mathematica notebook that is supplementary material for the research article ‘The Magnetic Field from Cylindrical Arc Coils and Magnets: A Compendium with New Analytic Solutions for Radial Magnetisation and Azimuthal Current’. The notebook contains all analytic solutions from the article in an electronic form. The purpose is to aid reproducibility of equations (in the unfortunate event of a typographical error) and provide validation test cases for the reader. The reader should have confidence in the results before adopting them. The notebook requires a Nomenclature package, that is consistent with the research article notation (found in the Appendix).
 
Basic wrapper functions for the analytic solutions are provided to tabulate results at specific field points, along with a sanity check comparison to the original numeric integral. All tabulated results were compared to an FEA model within the research article. Reviewers of the research article were interested in a basic computational benchmark of the analytic solution results compared to the numerical integral solution and FEA. Partial code to this effect is included. Further to the tables, some example plots are provided to visualise: the geometry; the location of the chosen field points; the magnitude and direction of the field passing through the geometry.

An extension of this work can be found in the [mag-gmst-force](https://github.com/AUMAG/mag-gmst-force) repository.

<img style="background-color:white;" src=https://github.com/AUMAG/mag-cyl-field/blob/main/doc/graphical-abstract.svg />

## Manuscript

This repository contains the results of the derivations presented in the following paper ([10.1002/apxr.202300136](https://doi.org/10.1002/apxr.202300136), [preprint](https://github.com/AUMAG/mag-cyl-field/blob/main/mag-cyl-field-Forbes-manuscript-2024.pdf)):

> M. Forbes, W.S.P Robertson, A.C. Zander, J.J.H. Paulides (2024) "The Magnetic Field from Cylindrical Arc Coils and Magnets: A Compendium with New Analytic Solutions for Radial Magnetisation and Azimuthal Current" *Advanced Physics Research* vol. 3, no. 7, p. 2300136.

## Citation

If you use this work, please cite the paper using the following reference:

    @Article {Forbes2024,
        author = {Forbes, M. and Robertson, W.S.P. and Zander, A.C. and Paulides, J.J.H},
        journal = {Advanced Physics Research},
        title = {The Magnetic Field from Cylindrical Arc Coils and Magnets: {A} Compendium with New Analytic Solutions for Radial Magnetisation and Azimuthal Current},
        doi = {10.1002/apxr.202300136},
        publisher = {Wiley},
        number = {7},
        pages = {2300136},
        volume = {3},
    }


## Mathematica files

While the typeset version of the paper presents the derivations and final solutions,
these are likely to be more accessible for re-use in the following Mathematica documents:

* [Nomenclature.wl](https://github.com/AUMAG/mag-cyl-field/blob/main/Nomenclature.wl)
* [Analytic solutions with numeric comparison.nb](https://github.com/AUMAG/mag-cyl-field/blob/3a05ef040823f40db63472190d9eeee88372eede/original-supporting-information/Analytic%20solutions%20and%20numeric%20comparison.nb)

There is a typeset PDF version of the latter notebook, but please note that the code is truncated at the page edge so it is intended purely for illustrative purposes.

The original supplementary material was updated to include geometry and field visualisations, following an invitation to share the Mathematica notebook on the Wolfram community (https://community.wolfram.com/groups/-/m/t/3222778):  
* [The Magnetic Field from Cylindrical Arc Coils and Magnets.nb](https://github.com/matt4bs/mag-cyl-field/blob/56f40633b3a6f32cd2976991ed63f2b000e11b34/The%20Magnetic%20Field%20from%20Cylindrical%20Arc%20Coils%20and%20Magnets.nb)

The authors extended the work herein to calculate the force and torque between collections of cylindrical arc coils and magnets. A simple package containly only the analytic magnetic field functions from this research output can be found:
* [MagCylField.wl](https://github.com/AUMAG/mag-cyl-field/blob/3a05ef040823f40db63472190d9eeee88372eede/MagCylField-package/MagCylField.wl)
* [MagCylField example.nb](https://github.com/AUMAG/mag-cyl-field/blob/3a05ef040823f40db63472190d9eeee88372eede/MagCylField-package/MagCylField%20example.nb)
