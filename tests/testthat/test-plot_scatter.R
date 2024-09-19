test_that("ScatterPlot function test cases", {
  # Create a sample dataset
  test_data <- data.frame(
    Peak_Expiratory_Flow = c(250, 300, 280),
    Height = c(160, 170, 165),
    Treatment = c('Placebo', 'Drug', 'Placebo')
  )
  
  # Test if the function runs without errors
  expect_no_error(ScatterPlot(
    data = test_data,
    x = Peak_Expiratory_Flow,
    y = Height,
    fill = Treatment
  ))
  
  # Test if the return value is a ggplot object
  expect_true(is.ggplot(
    ScatterPlot(
      data = test_data,
      x = Peak_Expiratory_Flow,
      y = Height,
      fill = Treatment
    )
  ))
  
  # Test if the x-axis variable is set correctly
  expect_equal(ggplot_build(
    ScatterPlot(
      data = test_data,
      x = Peak_Expiratory_Flow,
      y = Height,
      fill = Treatment
    )
  )$plot$labels$x,
  "Peak Expiratory Flow")
  
  # Test if the y-axis variable is set correctly
  expect_equal(ggplot_build(
    ScatterPlot(
      data = test_data,
      x = Peak_Expiratory_Flow,
      y = Height,
      fill = Treatment
    )
  )$plot$labels$y,
  "Height (cm)")
  
  # Test if the fill color variable is set correctly
  expect_equal(ggplot_build(
    ScatterPlot(
      data = test_data,
      x = Peak_Expiratory_Flow,
      y = Height,
      fill = Treatment
    )
  )$data[[1]]$fill,
  c("#fc8d59", "#91bfdb", "#fc8d59"))
  
  
})