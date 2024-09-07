module Coordinates where

import           Monomer (Point (Point), Rect (Rect), Size (Size))

-- | Перетворення координат Monomer до координат OpenGL
monomerToGl :: Size -> Point -> (Double, Double)
monomerToGl (Size winW winH) (Point x y) = (glX, glY)
  where
    glX = -1 + 2 * x / winW
    glY = 1 - 2 * y / winH

viewportToWindow :: Size -> Rect -> (Double, Double) -> (Double, Double)
viewportToWindow winSize@(Size winW winH) _nodeContentArea@(Rect rx ry rw rh) (x,y) =
  let (transX, transY) = monomerToGl winSize (Point (rx + rw/2) (ry + rh/2)) in
  ((x - transX) * rw/winW, (y - transY) * rh/winH)


-- | 6 vertices to draw a whole 2D texture. Shoulb be used with GL_TRIANGLES
textureCoords :: [(Point, Point)]
textureCoords = map (\(x,y,u,w) -> (Point x y, Point u w))
                [ (-1, -1, 0, 0), (-1, 1, 0, 1), (1, 1, 1, 1),
                  (1, 1, 1, 1), (1, -1, 1, 0), (-1, -1, 0, 0)]
