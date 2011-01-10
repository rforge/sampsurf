
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
In general we are interested in determing the properties of various sampling methods mentioned above when applied to fixed objects such as standing trees or down logs. The sampSurf package will allow generation of log or tree populations within a fixed tract area. The surface generated from the intersection of inclusion zones applied to the individuals in the population for a given attribute (e.g., cubic volume, number of individuals, etc.) are represented by the "sampSurf" class and can be displayed graphically. Estimator variance is directly associated with the sampling surface roughness, and so methods can also be compared visually. An example of a graphical depiction of a sampling surface using the "sausage" method may be viewed <a href="./ssExample.pdf">here</a>.
</p>

<p>
Please note that sampSurf is functional, but at the present only has support for down logs sampled on fixed-area plots (which include the ‘stand-up,’ ‘sausage,’ and ‘chainsaw’ methods or protocols). The class structure is designed to support additions with relative ease, however, so more will appear in future releases (hopefully with some contributed by users).
</p>



<br><br>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
