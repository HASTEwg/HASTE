# HATS

The High-Altitude Transport to Space (HATS) estimator is a high fidelity Monte Carlo code written in modern Fortran for estimating the radiation field seen by a space-based detector from a point source in or above the atmosphere.  The HATS code includes models for many of the special features of this air-to-space transport problem that are not modeled, or only partially modeled, in other transport codes.  Special features of the air-to-space transport problem include:
- Continuous treatment of the atmospheric density function
- Relative motions of source, detector, scattering medium (Earth rotation, athmospheric motion, thermal vibration of scattering nuclei)
- Gravity
- Radioactive decay of free neutrons

This code was originally developed in support of dissertation research at the Air Force Institute of Technology, and now remains under collaborative development in the radiation effects and detection community.

## Roadmap

#### v1.0
- [ ] On-the-fly (OTF) reconstruction of resonance cross sections
- [ ] High-altitude atmosphere implementation
- [ ] Tabulated source spectra
- [ ] Exoatmospheric sources
- [ ] Compiler portability
#### v1.1
- [ ] Direct computation of direct contribution
- [ ] History trace sample data for visualizations
- [ ] Detector constellations
- [ ] Pathlength integral checker option
- [ ] Primary and secondary photon transport
  - [ ] Photon sources
  - [ ] Photon transport
  - [ ] Photon detectors
#### v1.2
- [ ] Atmospheric detectors
- [ ] Low-altitude hydrogen
- [ ] On-the-fly (OTF) Doppler broadening enhancement & optimization
- [ ] Curved paths between atmospheric scatters
#### v1.3
- Run continuation & merging options
- Importance sampling for mitigating 'large' events
- Full ENDF "tape" format for cross sections data
#### v2.+
- [ ] Automated plotting & visualization packages
  - [ ] Real-time
  - [ ] Post-processing
- [ ] Thermal neutron decay zone/layer mapping
- [ ] OpenMP (or other) threading options
