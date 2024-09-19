userCard <-  function(...) {
  HTML(
    '
  <html lang="en">
  <head>
  <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Profile Card</title>
      <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
        </head>
        <body>
        <div class="profile-card">
          <img src="https://via.placeholder.com/100" alt="Profile Image">
            <div class="profile-content">
              <h2>Gareth Burns</h2>
              <p>Data Scientist, Exploristics</p>
              <div class="social">
                <a href="#"><i class="fa fa-github"></i></a>
                  <a href="#"><i class="fa fa-twitter"></i></a>
                    <a href="#"><i class="fa fa-linkedin"></i></a>
                      </div>
                      </div>
                      </div>
                      <!-- Font Awesome Icons -->
                      <script src="https://kit.fontawesome.com/a076d05399.js" crossorigin="anonymous"></script>
                        </body>
                        </html>'
  )
  
}