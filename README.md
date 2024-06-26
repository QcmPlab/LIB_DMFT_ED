# EDIpack2.0: Massively parallel Exact Diagonalization for generic Quantum Impurity problems

[![TestSuite](https://img.shields.io/github/actions/workflow/status/QcmPlab/EDIpack2.0/PushWorkflow.yml?label=TestSuite&logo=Fortran&style=flat-square)](https://github.com/QcmPlab/EDIpack2/actions/workflows/PushWorkflow.yml) 

<!-- TO BE SETUP ASAP
[![Coverage]()]()
[![api docs](https://img.shields.io/static/v1?label=API&message=documentation&color=734f96&logo=read-the-docs&logoColor=white&style=flat-square)](https://qcmplab.github.io/DMFT_ED)
-->
A suitable extension of [EDIpack](https://arxiv.org/abs/2105.06806): a  Lanczos based method 
for the solution of generic Quantum Impurity problems,  exploiting distributed memory MPI parallelization.
This updated version, aims to solve single-site, multi-orbital models, in either  *normal*, *superconducting* (s-wave) or *Spin-non-conserving* (e.g. with Spin-Orbit Coupling or in-plane magnetization) phases, including electron-phonons coupling. The code works at zero and low temperatures.   
 
See [j.cpc.2021.108261](https://doi.org/10.1016/j.cpc.2021.108261) for further information about the underlying algorithms. Yet, suitable modifications have been developed to address the Superconducting and non-SU(2) channels.  


### Dependencies

The code is based on:  

* SciFortran [https://github.com/QcmPlab/SciFortran](https://github.com/QcmPlab/SciFortran)  

* MPI 

  


### Installation

Installation is available using CMake. In the current version API are only provided in Fortran. In a future release Python and C/C++ API will be included.  
The software gives acces to the static library `libedipack2.a` and the related modules `EDIPACK2`

Clone the repo:

`git clone https://github.com/QcmPlab/EDIpack2.0 EDIpack2`

And from the repository directory (`cd EDIpack2`) make a standard out-of-source CMake compilation:

`mkdir build`
`cd build`
`cmake ..`     
`make`     
`make install`   
`make post-install`    

Please follow the instructions on the screen to complete installation on your environment.  
The library can be loaded using one of the following, automatically generated, files :  

* pkg-config file in `~/.pkg-config.d/EDIpack2.pc`  
* environment module file `~/.modules.d/EDIpack2/<PLAT>`  
* homebrew `bash` script `<PREFIX>/bin/configvars.sh`


The `CMake` compilation can be controlled using the following additional variables, default values between `< >`:   

* `-DPREFIX=prefix directory <~/opt/EDIpack2/VERSION/PLAT/[GIT_BRANCH]>` 

* `-DUSE_MPI=<yes>/no`  

* `-DVERBOSE=yes/<no> `  

* `-DBUILD_TYPE=<RELEASE>/TESTING/DEBUG`  

--

***LICENSE***  
Copyright 2020- (C) Adriano Amaricci, Lorenzo Crippa, Alberto Scazzola, Gabriele Bellomia, Samuele Giuli, Giacomo Mazza, Francesco Petocchi, Luca de Medici and Massimo Capone

The software is provided with no license, as such it is protected by copyright.
The software is provided as it is and can be read and copied, in agreement with 
the Terms of Service of GITHUB. Use of the code is constrained to author agreement.   


This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.


