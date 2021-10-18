<?php
  	session_start();
	if (!isset($_SESSION["uname"]))
	{
		header("location:login.php");
	}
	else
	{
		require_once("header.php");?>
		<div class="row">
      <div class="panel twelve columns text-center"><br/>
        <h3>Weekly Unemployment Analysis</h3>
          <hr>
          <img src="img/Unemployment Analysis.jpeg"><br><br>
      <a class="button" href="index.php">Home</a></li>
      </div>
    </div>
<?php
	}
	include_once("footer.php");
?>