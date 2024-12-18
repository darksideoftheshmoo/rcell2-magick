# rcell2.magick - Magick and Shiny tools for cytometry from microscopy data

This package provides:

* `magick`-based functions to prepare tiled images of single cells, over imaging channels and time frames.
  * It requires [ImageMagick](https://imagemagick.org/script/download.php) in your system.
* Shiny apps to filter and annotate cytometry datasets graphically, with live image previews of the cells.

rcell2's full functionality is split into four packages:

* The main rcell2 package offers functions to load Cell-ID's output to data.frames, and image manipulation based on EBImage. A development version of this package is available in the [`rcell.dev`](https://github.com/darksideoftheshmoo/rcell2/tree/rcell2.dev) branch.
* Cell-ID, the image segmentation software, has been wrapped in the [`rcell.cellid`](https://github.com/darksideoftheshmoo/rcell2/rcell2-cellid) package. It offers functions to run CellID from R, and an rmarkdown template showcasing advanced functionality.
* The cell tiling and graphic filtering apps, built on R-Shiny and [magick](https://github.com/ropensci/magick), are available in the [`rcell.magick`](https://github.com/darksideoftheshmoo/rcell2-magick) package.
* The [`rcell2.examples`](https://github.com/darksideoftheshmoo/rcell2.examples) package contains notebooks on general usage, and on several classification and analysis methods.

This package suite is very well tested in baker's yeast data, and R version 4+.

## Main functions

Brief description of what the package does, and how to use it.

### Usage examples

An Rmd notebook with minimal and detailed usage examples is available.

It is included as an Rmarkdown template,
and can also be opened in Rstudio with a convenience function:

```r
get_workflow_template_magick()
```

This will either copy or download and open a Rmarkdown notebook,
with usage examples and brief explanations.


### Magick functions

The magick functions display single cell images from microscopy datasets. See:

* `rcell2.magick::magickCell()`: main image tile generator.
* `rcell2.magick::cellStrip()` and `rcell2.magick::cellStrips()`: multichannel image strips of single cells (example below).
* `rcell2.magick::square_tile()`: square image tile of single cells (example below).
* `rcell2.magick::cellSpread()` and `rcell2.magick::cellSpreadPlot()`: square tile or ggplot object, showing samples images of cells, 2D-binned over custom variables.

![analisis_Far1_arresto-lavado-ucid20254_PRE_a502060358b](https://user-images.githubusercontent.com/3259326/184430766-f2321758-2f7b-4d99-a6fe-44c7d72e56e2.png)

> Cell strip.

![image](https://user-images.githubusercontent.com/3259326/184431579-88049690-46fc-4f55-962b-03e95f048314.png)

> Spread plot.

### Shiny apps

* `rcell2.magick::shinyCell()`: An R-Shiny app will help users filter data graphically, with live image previews. This app is general purpose (i.e. useful in standard cell cytometry).
* `rcell2.magick::tagCell()`: app to "tag" single cells in the dataset with user defined options.
* `rcell2.magick::plotApp()`: a small app to filter a dataframe graphically.

![image](https://user-images.githubusercontent.com/3259326/184431764-7cf694ba-a30d-4207-948e-21a1fcc5eb82.png)

> shinyCell

![image](https://user-images.githubusercontent.com/3259326/184432557-6f0e6104-51b0-40d0-a881-1f0463f7a645.png)

> tagCell

## Installation

### R Dependencies

Most of the dependencies are listed in the `DESCRIPTION` file, and should install automatically.

In a Mac OS computer, the binary `magick` package may fail to annotate images, with the following error message:

```
Error: rsession-arm64: NonconformingDrawingPrimitiveDefinition `text' @ error/draw.c/RenderMVGContent/4456
```

To fix this, re-install the `magick` package "from source". This requires the ImageMagick library to be installed in your system, as described below.

### System dependencies

Install `imagemagick` on your system; this is required by R's `magick` package. All the major operating systems are supported by ImageMagick. See: <https://imagemagick.org/script/download.php>

#### macOS

To install ImageMagick in macOS you will need to:

- Install the `brew` package manager: <https://brew.sh/>
- Use brew to install the `imagemagick@6` package: <https://github.com/ropensci/magick#installation>

More information at: <https://imagemagick.org/script/download.php>

#### Linux

For Ubuntu and Arch Linux these commands may come in handy:

```sh
# Aptitude
sudo apt install imagemagick libmagick++-dev

# Pacman
sudo pacman -S imagemagick
```

### Installing the package

Install the package using `remotes`. This will fetch the latest version directly from its GitHub repository:

```r
remotes::install_github("darksideoftheshmoo/rcell2-magick")
```

---

!['Automating' comes from the roots 'auto-' meaning 'self-', and 'mating', meaning 'screwing'.](https://imgs.xkcd.com/comics/automation.png)

