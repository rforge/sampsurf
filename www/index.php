
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<!-- this is the original version, which displays a link rather than the logo... -->
<!-- <a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr> -->  
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<!-- Information about the package added 17-Dec-2010 -->
<p> 
The sampSurf package can be used to construct sampling surface simulations for different areal sampling methods common to forestry and ecology. These would include fixed-area plot methods, line intersect sampling, and various variable-radius plot sampling methods, the latter more commonly used in forestry. Most of these fall under the general probability proportional to size (PPS) umbrella and have some form of inclusion zone associated with each individual in the population. The inclusion zone has well-defined area (sometimes called the inclusion area) and may be thought of simply as that zone within which a sample point could fall and select the associated individual. The sample point can be the center of a circular fixed-area plot, an actual point as in horizontal point sampling, or say, the midpoint of a line in line intersect sampling. 
</p>

<p>
In general we are interested in determining the properties of various sampling methods mentioned above when applied to fixed objects such as standing trees or down logs. The sampSurf package will allow generation of log or tree populations within a fixed tract area. The surface generated from the intersection of inclusion zones applied to the individuals in the population for a given attribute (e.g., cubic volume, number of individuals, etc.) are represented by the "sampSurf" class and can be displayed graphically. Estimator variance is directly associated with the sampling surface roughness, and so methods can also be compared visually. An example of a graphical depiction of a sampling surface using the "sausage" method may be viewed <a href="./ssExample.pdf">here</a>.
</p>

<p>
There are several vignettes associated with the package that provide detailed explanation and examples of the design and use of the various components. A good place to start is with <em>The sampSurf Package Overview</em> vignette, which provides an overview of the class structure, etc. The vignettes are all available from the package index help page once the package has been installed and loaded using library(). Another overview of the package in the help system can be found by issuing "package?sampSurf" (without the quotes) at the R command line. This help page will provide links to all of the built-in class structure, generics with associated methods, and helper functions. Running the examples (with R's example() command) will also provide some help in getting started. 
</p>

<p>
Please note that sampSurf is functional, but at the present only has support for down logs sampled on fixed-area plots (which include the ‘stand-up,’ ‘sausage,’ and ‘chainsaw’ methods or protocols). The class structure is designed to support additions with relative ease, however, so more will appear in future releases (hopefully with some contributed by users).
</p>

<h4>A Note On Installation</h4>


<p>
On the project pages, you will note that you can install the package directly from R-Forge using...<br><br>

install.packages("sampSurf", repos="http://R-Forge.R-project.org")<br><br>

and can include the dep=TRUE argument if you want the packages that sampSurf is dependent upon to also be installed. Unfortunately, installing the dependencies in this way may be a little difficult. First, I believe that the dependencies are only searched for on R-Forge, and not CRAN (you could add more repos to the list to compensate). Since raster and rgl are both R-Forge projects, they will be found. But gpclib, for example, is not on R-Forge and therefore R will complain that it can not find it. Second, the packages that are available on R-Forge could be in some intermediate stage of update, so it is better to install the required packages from CRAN, with what is known to be a stable release. The dependencies are listed in the DESCRIPTION file for sampSurf; they are: sp, raster, gpclib, with rgl suggested but not necessary unless you want to look at the sampling surfaces in 3D. Therefore, I would recommend that you use the normal install.packages() on all these packages so that you get the latest stable version from CRAN. Here is what I would try (in this order)...<br><br>

install.packages("sp")<br>
install.packages("raster")<br>
install.packages("gpclib")<br>
install.packages("sampSurf", repos="http://R-Forge.R-project.org")<br><br>

This should do it and get the correct stable releases from CRAN prior to installing sampSurf. Please note the limited use license restriction on gpclib (see this package on CRAN).
</p>


<h5>rgl Installation</h5>
<p>
Finally, on Linux, the rgl package requires system dependencies. The freeglut and mesa libraries must be installed. But in addition, the <em>development</em> versions of freeglut and libpng must also be install for rgl to run. On Fedora 12, these were freeglut-devel and libpng-devel which could be installed with yum prior to installing rgl within R. If you use windows, unfortunately, you are on your own (but please let me know so I can post instructions here for others if there is anything extra one has to do).
</p>




<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
