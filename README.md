# HATS

The High-Altitude Transport to Space (HATS) estimator is a high fidelity Monte Carlo code written in modern Fortran for estimating the radiation field seen by a space-based detector from a point source in or above the atmosphere.  The HATS code includes models for many of the special features of this air-to-space transport problem that are not modeled, or only partially modeled, in other transport codes.  Special features of the air-to-space transport problem include:
- Continuous treatment of the atmospheric density function
- Relative motions of source, detector, scattering medium (Earth rotation, athmospheric motion, thermal vibration of scattering nuclei)
- Gravity
- Radioactive decay of free neutrons

This code was originally developed in support of dissertation research at the Air Force Institute of Technology, and now remains under collaborative development in the radiation effects and detection community.  Roadmap for feature development is [in the wiki](https://github.com/HATSwg/HATS/wiki/Roadmap).
