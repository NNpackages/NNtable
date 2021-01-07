<p>

![NNtable](./man/figures/logo.png =110x)<span style="font-size:100px"><sup><font color="DarkBlue">NN</font>table</sup></span>
</p>

<!-- badges: start -->
[![Build Status](https://novonordiskit.visualstudio.com/BOS/_apis/build/status/NNtable?branchName=master)](https://novonordiskit.visualstudio.com/BOS/_build/latest?definitionId=1152&branchName=master)
[![Board Status](https://novonordiskit.visualstudio.com/3d1f3d66-ac3d-4106-a6cd-7035e84f3854/da722b93-da4d-4626-9151-4b5bd40235fa/_apis/work/boardbadge/347a30bf-c638-4953-83e1-282af9cca607?columnOptions=1)](https://novonordiskit.visualstudio.com/3d1f3d66-ac3d-4106-a6cd-7035e84f3854/_boards/board/t/da722b93-da4d-4626-9151-4b5bd40235fa/Microsoft.RequirementCategory/)
[![Release Status Prod](https://novonordiskit.vsrm.visualstudio.com/_apis/public/Release/badge/5e599944-02cf-4939-8515-28d5f82d110a/29/191)](https://novonordiskit.visualstudio.com/SCE-R/_release?_a=releases&view=mine&definitionId=29)
<!-- badges: end -->

The **R**-package **NNtable** supplies users with functions for creating tables that can be used in NN TFL. 

The philosophy behind NNtable is to separate the derivation of statistics and the layout of an output table. All layout specic derivations are supposed to be handled by the NNTable functionality.

Current webpage: [NNtable](http://10.59.86.7/NNpackages/NNtable/)

----
## Installation 
The package can be installed directly from RStudios package management system


### **R**-server

On the R-server an updated set of packages can be installed by the following command:
```r
install.packages("NNtable") 
```

### Local laptop

On your local laptop an updated set of packages can be installed by the following command:
```r
install.packages("NNtable", repos = "https://rspm.bifrost-prd.corp.aws.novonordisk.com/cran-internal-prod/latest") 
```

Remember that R-tools need to be installed and paths need to be included in windows paths variable.

----
## Contribute

We welcome all to contribute to the **NNtable** project. If you want to contribute,
note that we try to adhere to the [GitHub flow](https://guides.github.com/introduction/flow/) model. I.e. the master
branch is always deployable. Hence, we request that you use git branches and merge requests.

The following lists the requirements
* The master branch is always deployable (i.e. as a minimum passing R CMD checks and unit tests)
* Use git feature/bugfix branches and merge requests to the master
* [Semantic versioning](https://semver.org/)
* Follow the used coding style and use Rstudio's code styling diagnostics.
* Remember to do an R CMD check before committing.
* Always consider writing some [unit tests](http://r-pkgs.had.co.nz/tests.html) for your new functionality. 
* Unlike SAS - R code should fail or warn loudly and proudly if input is ambiguous or unexpected.
