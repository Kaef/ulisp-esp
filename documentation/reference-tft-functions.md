# TFT functions (ESP-WROVER-KIT only)

The following functions may work with other displays too, but this is not tested yet.

* scroll

    scrolls display in y-direction upwards for given number of lines with given background color (565 format)
    ```
    (scroll 100)      -- scroll 100 lines upwards
    (scroll 100 987)  -- scroll 100 lines upwards, fill scrolled room with color 987
    ```

* setCursor

    sets the cursor to given position
    Origin (0 0) is the lower left corner.
    ```
    (setCursor 100 200) -- sets the current cursor position to x=100, y=200
    ```

* plot

    plots a point at position (x y) with color given in 565 format
    if no color given, the last used color will be used (defaults to white)
    ```
    (plot 100 200 #xFFFF) -- plots a white dot at x=100, y=200
    (plot 100 150 (color565 255 0 0)) -- plots a red dot at x=100, y=150 using library function (color565) to set color to red
    ```

    Here's a simple example:
    ```
    (defun graftest ()
      (require 'color565)
      (scroll 319)
      (setCursor 50 250)
      (princ "uLisp -- plot demo I")
      (dotimes (x 240)
        (plot x x (color565 255 0 0))
        (plot x (- 240 x) (color565 0 255 0)))
      (let ((color (color565 255 255 0)))
        (dotimes (x 240)
          (plot x 1 color)
          (plot x 240)
          (plot 0 x)
          (plot 239 x)))
      (scroll 10)
    (setCursor 0 8))
    ```

  * setTextColor

    Set's text foreground- and background-colors to the given colors.
    The colors must be given in 565 format, you can use the library function (color565 r g b)
    ```
    (require 'color565)
    (setTextColor (color565 255 255 0) (color565 0 0 255)) -- sets the color to yellow on blue
    ```

* readPixel

    reads the pixel color at position x, y; returns color565
  

