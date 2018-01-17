module Main where

import Prelude
import Element as J
import Control.Monad.Eff
import Control.Monad.ST
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, on, getPageX, getPageY, body, closest)
import OutWatch.Helpers.Helpers
import Math
import Global.Unsafe

import Data.Int
import Data.Traversable (for)
import Data.Array
import Data.Maybe
import Data.Array.ST
import Data.Foreign (Foreign)

import Graphics.Canvas
import Graphics.Drawing(render)
import Partial.Unsafe (unsafePartial)

import Control.Monad.Eff.Console (CONSOLE,log,logShow)
import Control.Monad.Eff.Ref

import DOM.RequestAnimationFrame
import DOM (DOM)

drawLine sx sy ex ey ctx =  strokePath ctx $ do
  _ <- setStrokeStyle "#FF00FF" ctx
  _ <- moveTo ctx sx sy
  _ <- lineTo ctx ex ey
  _ <- closePath ctx
  r <- emptySTArray
  void $ pushSTArray r 1

rotateX angle x y z = do
  let rad = angle * pi / 180.0
  let yy = y * (cos rad) - z * (sin rad)
  let zz = y * (sin rad) + z * (cos rad)
  [x,yy,zz]

rotateY angle x y z = do
  let rad = angle * pi / 180.0
  let xx = z * (sin rad) + x * (cos rad)
  let zz = z * (cos rad) - x * (sin rad)
  [xx,y,zz]

dampingfactor :: Number -> Number
dampingfactor x = x * 0.9

drawCube st_nodes st_edges ctx = void $ forE 0 12 $ \i ->  do
    --log "DrawCube"
    ----------------Current edge
    i_edge_temp <- peekSTArray st_edges i    -- Read the value at the specified index in a mutable array.
    let i_edge = fromMaybe [] i_edge_temp    --Converts  maybe to array

    let i_edge_s = i_edge !! 0  --Read the value at index returns Maybe
    let i_edge_e = i_edge !! 1
    let e_s = fromMaybe 0 i_edge_s -- Converts maybe to int
    let e_e = fromMaybe 0 i_edge_e


    --------------- Start node
    s_node_temp <- peekSTArray st_nodes e_s
    let s_node = fromMaybe [] s_node_temp

    let s_x_temp = s_node !! 0
    let s_y_temp = s_node !! 1
    let s_x = fromMaybe 0.0 s_x_temp
    let s_y = fromMaybe 0.0 s_y_temp

    ----------------- End node
    e_node_temp <- peekSTArray st_nodes e_e
    let e_node = fromMaybe [] e_node_temp

    let e_x_temp = e_node !! 0
    let e_y_temp = e_node !! 1
    let e_x = fromMaybe 0.0 e_x_temp
    let e_y = fromMaybe 0.0 e_y_temp

    drawLine s_x s_y e_x e_y ctx

---- Mouse events
mouseDown input oldx oldy rx ry dragging e _ =  do
  rxV <- readSTRef rx
  ryV <- readSTRef ry
  _ <- writeSTRef rx (rxV/5.0)
  _ <- writeSTRef ry (ryV/5.0)
  pagex <- J.getPageX e
  pagey <- J.getPageY e
  _ <- writeSTRef dragging true
  --logShow ((pagex))
  --logShow ((pagey))
  _ <- writeSTRef oldx {-((pagex -300.0)/300.0)-} pagex
  _ <- writeSTRef oldy {-((600.0 - pagey - 300.0)/(300.0))-} (600.0 -pagey)
  pure unit

mouseMove input rxd ryd rx ry dragging e _ =  do
 drag <- readSTRef dragging
 if drag 
  then do
   pagex <- J.getPageX e
   pagey <- J.getPageY e
   --logShow ((pagex))
   --logShow ((pagey))
   rxdm <- readSTRef rxd
   rydm <- readSTRef ryd
   _ <- writeSTRef ry ((pagex - rxdm)*2.0)
   _ <- writeSTRef rx (((600.0 - pagey) - rydm)*2.0)
   _ <- writeSTRef rxd {-((pagex -300.0)/300.0)-} pagex
   _ <- writeSTRef ryd {-((600.0 - pagey - 300.0)/(300.0))-} (600.0 -pagey) 
   --logShow ((pagex - rxdm)/10.0)
   --logShow (((600.0 - pagey) - rydm)/10.0)
   pure unit
  else pure unit

mouseUp input rxd ryd rx ry dragging e _ =  do
  writeSTRef dragging false

main :: forall a b.       
        Eff                   
          ( st :: ST a     
          , canvas :: CANVAS  
          , console :: CONSOLE
          , dom :: DOM        
          | b              
          )                   
          Unit    
main = void $ unsafePartial do
  rxd <-newSTRef 0.0
  ryd <- newSTRef 0.0
  rx <- newSTRef 0.0
  ry <- newSTRef 0.0
  dragging <- newSTRef false

  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  canvas_jq <- J.getElementById "canvas"  
  canvas_body <- J.getElementById "body"

  _ <- translate { translateX: 250.0, translateY:  250.0 } ctx -- translating the canvas

  ------------------- Normal arrays
  let sz = 100.0
  let msz = -100.0
  let nodes = [
        [msz, msz, msz],
        [msz, msz, sz],
        [msz, sz, msz],
        [msz, sz, sz],
        [sz, msz, msz],
        [sz, msz, sz],
        [sz, sz, msz],
        [sz, sz, sz]
  ]
  let edges = [[0, 1],[1, 3],[3, 2],[2, 0],[4, 5],[5, 7],[7, 6],[6, 4],[0, 4],[1, 5],[2, 6],[3, 7]]

  -----------------Converting normal arrays to st_arrays
  -- #Nodes
  st_nodes <- emptySTArray

  void $ forE 0 8 $ \i ->  do
    log "Resetting"
    let xx = (nodes !! i)
    let yy = fromMaybe [] xx
    void $ pushSTArray st_nodes yy
  -- #Edges
  st_edges <- emptySTArray
  void $ forE 0 12 $ \i ->  do
    let xx = (edges !! i)
    let yy = fromMaybe [] xx
    void $ pushSTArray st_edges yy


  --------------------Updating cube using renderAnimation
  let updateCube = do
	--log "Sid"
	--void $ forE 0 8 $ \i ->  do
        --  mvi <- peekSTArray st_nodes i
        --  let vi = fromMaybe [] mvi
	--  logShow vi
	--  pure unit
	st_nodes_rotated <- emptySTArray
        void $ forE 0 8 $ \i ->  do
          mvi <- peekSTArray st_nodes i
          let vi = fromMaybe [] mvi
	  --logShow vi
          let mx = vi !! 0
          let my = vi !! 1
          let mz = vi !! 2
          let x = fromMaybe 0.0 mx
          let y = fromMaybe 0.0 my
          let z = fromMaybe 0.0 mz
          xangle <- (readSTRef rx) -- Angle
          yangle <- (readSTRef ry) -- Angle
          let v = rotateX (xangle) x y z
          let mvx = v !! 0
          let mvy = v !! 1
          let mvz = v !! 2
          let vx = fromMaybe 0.0 mvx
          let vy = fromMaybe 0.0 mvy
          let vz = fromMaybe 0.0 mvz
          let vv = rotateY (yangle) vx vy vz

	  void $ pushSTArray st_nodes_rotated vv
	xangle <- (readSTRef rx) -- Angle
        yangle <- (readSTRef ry) -- Angle  	
	void $ writeSTRef rx (dampingfactor $ xangle)
        void $ writeSTRef ry (dampingfactor $ yangle)  
	--logShow xangle
        --logShow yangle

	void $ forE 0 8 $ \i -> do
	  mvi <- peekSTArray st_nodes_rotated i
          let vi = fromMaybe [] mvi
	  void $ modifySTArray st_nodes i $ \mf->vi

        width <- getCanvasWidth canvas
        height <- getCanvasHeight canvas
        void $ clearRect ctx { x: -300.0, y: -300.0, w: width, h: height }

	drawCube st_nodes_rotated st_edges ctx

	requestAnimationFrame updateCube
  updateCube
  J.on "mousedown" (mouseDown canvas rxd ryd rx ry dragging) canvas_jq
  J.on "mousemove" (mouseMove canvas rxd ryd rx ry dragging) canvas_jq
  J.on "mouseup" (mouseUp canvas rxd ryd rx ry dragging) canvas_body

