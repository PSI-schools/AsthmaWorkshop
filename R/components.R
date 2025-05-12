userCard <-  function(name = "Gareth Burns", job_title = "Data Scientist", company = "Exploristics", image = "https://via.placeholder.com/80", ...) {
    div(class = "user-card",
        style = "border: 1px solid #ddd; padding: 20px; border-radius: 10px; display: flex; align-items: center;",
        img(src = image, style = "width: 80px; height: 80px; border-radius: 50%; margin-right: 20px;"),
        div(
          strong(name, style = "margin: 0;"),
          p(job_title, style = "margin: 0;"),
          p(company, style = "margin: 0;")
        )
    )
  }