# shinymrp 0.10.0

## New features
- Add ICAR model as a prior distribution option (#113)
- Add BYM2 to prior distribution options (#114)

## Other changes
- Open Shiny App in browser by default (#110)
- Add a vignette for spatial priors (#111)
- Replace links that block auomated HEAD requests (#112)


# shinymrp 0.9.1

## Bugfixes
- Fix variable misclassification (#107)

## Other changes
- Upgrade to qs2 (#108)
- Minor UI improvements (#109)

# shinymrp 0.9.0

## New features
- Create package API for implementing MRP workflow in R scripts (#88)
- Allow selection of uncertainty intervals in estimate plots (API only) (#99)
- Allow conversion of dates to month and year indices (#100)

## Bugfixes
- Fix non-character label of the button used for linking to ACS data (#87)
- Fix filtering of interactions for structured prior (#96)
- Prevent removal of 'date' column during data preprocessing (#97)

## Other changes
- Create pkgdown website (#81)
- Change plot y-axis label and select input in "Visualize data" tab (#86)
- Package data are either serialized or store in a separate data repository (#89, #101)
- Move cmdstanr to "Suggests" dependencies (#105)


# shinymrp 0.8.0

## New features
- Handle continuous outcome measure (#66)

## Bugfixes
- Fix incorrect order of levels in demographic distribution and estimate plots (#78)
- Fix model compilation error caused by empty prior input (#77)

# shinymrp 0.7.1

## Bugfixes
- Fix incorrect estimates caused by level mismatch between sample and poststratification data (#63)
- Fix model compilation errors caused by including multiple varying slopes with the same categorical variable (#58)
- Fix model compilation errors caused by interaction terms involving categorical variables included as fixed effects (#57)
- Bugfix for static LOO-CV table (#59)
- Fix error in "View results" tab caused by switching interface version (#62)
- Fix consecutive model tab removal error (#61)
- Bugfix for poststratification table upload for general time-varying data (#60)


# shinymrp 0.7.0

## New features
- Upgrade UI to Boostrap 5 standard (#42)
- Add diagnostics sections for model fitting and comparison (#40)
- Add poststratification table upload (#54)
- Add indicator for interface version/use case (#41)
- Add download button for preprocessed data (#46)
- Add back button and update use case description in the homepage ($48)
- Add reference levels in estimated parameter table (#49)

## Bugfixes
- Suppress console output from compilation of standalone generated quantities code (#39)
- Fix inconsistent ggplot styling on different machines (#28)
- Prevent crashing when errors occur during LOO-CV (#51)
- Omit interaction between nested variables (#50)

## Other changes
- Add documentation about input data requirements to in-app user guide (#31)
- Notify users of the omission of variables with only one level (#29)
- Minor UI improvements (#47)


# shinymrp 0.6.0

## New features
- Extend ACS-data-linking to county and state (#19)
- Add data linking section to data upload tab (#34)
- Add two new interface versions for general use cases (#35)

## Bugfixes
- Fix demographic distribution bar plot error (#27)

## Other changes
- Make fit object self-contained by adding preprocessed data, poststratification table, and metadata (#43)
- Change preprocessing code to include all tracts instead of only the ones in the urbanicity and ADI datasets (#24)
- Fix letter casing of graph labels for counties and state (#44)


# shinymrp 0.5.0

## New features
- Allow users to run poststratification separately (#20)
- Significantly speed up poststratification by running it through Stan (#21)

## Bugfixes
- Fix errors thrown by `renderPlot` when users switch interface version (#25)
- Conditional display of sections in table of estimates (#23)

## Other changes
- Speed up fit object uploading and downloading with `qs`


# shinymrp 0.4.0

## New features
- Allow fixed effects of categorical variables
- Filter effects compatible with structured prior
- Add prior syntax validation and warning
- Replace Learn > Interface with a popup "Guide" dialog

## Bugfixes
- Fix error that occurs when you run model fitting without including any effects
- Change confidence interval of posterior draw summary from 90 to 95

## Other changes
- Change variable select input update logic
- Minor UI changes

# shinymrp 0.3.1

## New features
- Add structure prior for interaction between sex and categorical variables
- Prevent a categorical from being included as both a fixed effect and a varying effect
- Add C++ toolchain checking and CmdStan installation
- Add model comparison to web version

## Bugfixes
- Fix arrow of summary HTML element not rendered on Window
- Fix missing covariates when using example individual-level cross-sectional data
- Variable select inputs persist after users navigate to another tab
- Fix error in model comparison tab when users change interface version

## Other changes
- Restrict categorical variables to varying effects in model specification
- Explicitly set ggplot font to ensure consistency across platforms
- Update Learn > Interface

# shinymrp 0.3.0

## New features
- Revamp model specification UI and Stan-code generating backend
- Add interaction terms to model specification
- Add structure prior
- Add random sampling for unseen levels

## Other changes
- Prevent users from proceeding to "Analyze" tab without selecting an interface version
- Update default priors
- Update Learn > Interface

# shinymrp 0.2.0

## New features
- Allow user to save and upload model fits
- Add buttons that allow user to use example data and model fits

## Bugfixes
- Fix model ordering in the LOO comparison table

## Other changes
- Feedback link takes users to a Google Form instead of GitHub Issue page


# shinymrp 0.1.0
Beta release
