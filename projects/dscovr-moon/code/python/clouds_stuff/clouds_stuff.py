#!/usr/bin/env python3

import math
from math import sqrt, sin, cos, tan, atan2, asin

import os
from threading import Thread
import multiprocessing
import queue

import numpy as np
import matplotlib.pyplot as plt

from skimage import io
from skimage import data, color
from skimage.transform import hough_circle, hough_circle_peaks, rescale
from skimage.feature import canny
from skimage.draw import circle_perimeter, set_color
from skimage.util import img_as_ubyte, img_as_uint


class Clouding:
  def __init__(self,fn,rscl=1,edgrscl=1):
    print("Clouding __init__()")
    self._fn=fn
    self._rescale   =rscl    # Rescale factor. 1 means 100%
    self._edgrescale=edgrscl # Rescale factor for edge and cirle detection
                             # will multiply the image factor.
    
    # Load picture and detect edges
    img0    = io.imread(self._fn, as_gray=True)
    imgRGB0 = io.imread(self._fn)

    self._img    = rescale(img0   , self._rescale, anti_aliasing=True                   )
    self._imgRGB = rescale(imgRGB0, self._rescale, anti_aliasing=True, multichannel=True)
    
  def findEdges(self):
    print("Clouding.findEdges(): START")
    self._edges = canny(rescale(self._img,self._edgrescale, anti_aliasing=True ))
    print("Clouding.findEdges(): END")
    
  def findCircle(self):
    print("Clouding.findCircle(): START")
    # Detect two radii
    hough_radii = np.arange(
      int(self._edgrescale*self._rescale*800), 
      int(self._edgrescale*self._rescale*1000), 2)
    hough_res = hough_circle(self._edges, hough_radii)
    # Select the most prominent 1 circles
    # The x/y inversion here seems needed.
    self._accums, self._cy, self._cx, self._radii = hough_circle_peaks(hough_res, hough_radii,total_num_peaks=1)
    # Scaling back. WARNING: works only for one circle
    self._cx   [0]=self._cx   [0]/self._edgrescale
    self._cy   [0]=self._cy   [0]/self._edgrescale
    self._radii[0]=self._radii[0]/self._edgrescale
    print("Clouding.findCircle(): END")

  def addCircle(self):
    print("Clouding.addCircle(): START")
    # Gots lots of problem with this, it seems to blacken the whole image
    # very often. That's why I put copy() everywhere
    # Problem, solved FIXME: remove all that
    self._imgRGBcircle=self._imgRGB.copy()
    for center_y, center_x, radius in zip(self._cy, self._cx, self._radii):
      print ("( "+str(center_x)+" , "+str(center_y)+" , "+str(radius)+" ) ")
      circx, circy = circle_perimeter(center_x, center_y, radius, shape=self._imgRGB.shape)
      #set_color(self._imgRGBcircle, (circx, circy) , [250, 50, 50])
      self._imgRGBcircle[circx,circy]=(.8, .2, .2)
      #print (self._imgRGBcircle[center_x,center_y])
      #print (self._imgRGBcircle[center_x+1,center_y+1])
      #self._imgRGBcircle[center_x,center_y]=(.8, .2, .2)
      #print (self._imgRGBcircle[center_x+1,center_y+1])
      #print (self._imgRGBcircle[center_x,center_y])
    #self._imgRGBcircle=self._imgRGBcircle.copy()
    print("Clouding.addCircle(): END")
      
    
  def drawCircle(self):
    fig, ax = plt.subplots(ncols=1, nrows=1, figsize=(10, 4))
    for center_y, center_x, radius in zip(self._cy, self._cx, self._radii):
      print ("( "+str(center_x)+" , "+str(center_y)+" , "+str(radius)+" ) ")
      circx, circy = circle_perimeter(center_x, center_y, radius, shape=self._imgRGB.shape)
      set_color(self._imgRGB, (circx, circy) , [250, 50, 50])

    ax.imshow(self._imgRGB, cmap=plt.cm.gray)
    plt.show()
  
    
  def processCircle(self):
    self.findEdges()
    self.findCircle()

  def fakeprocessCircle(self):
    self._cx   =[int(len(self._img   )/2)]
    self._cy   =[int(len(self._img[0])/2)]
    self._radii=[int(len(self._img   )/2.5)]
    print ("( "+str(self._cx[0])+" , "+str(self._cy[0])+" , "+str(self._radii[0])+" ) ")


  def processFlattenForward(self):
    self._flatten_image=0*self._imgRGB
    self._flatten_image=rescale(self._flatten_image,1.5, multichannel=True)
    cx=self._cx[0]
    cy=self._cy[0]
    r0=self._radii[0]
    
    # FIXME: there is some weird swapping of x/y coordinates
    # we'll deal with it for now.
    ym=cx-r0
    yM=cx+r0
    xm=cy-r0
    xM=cy+r0

    print ("Circle : ( "+str(cx)+" , "+str(cy)+" ) "+str(r0))
    for ix in range(xm,xM):
      for iy in range(ym,yM):
        #print("( "+str(ix)+" , " +str(iy)+" )")
        try:
          color=self._imgRGB[ix,iy]
          #print(color)
        except IndexError:
          print("That should not happens: ( "+str(int(ix))+" , " +str(int(iy))+" )")
          continue
        
        r=math.sqrt((cy-ix)*(cy-ix)+(cx-iy)*(cx-iy))
        theta=math.atan2(iy-cx,ix-cy)
        #print ("theta: "+str(theta*180/math.pi)+" x: "+str(ix-cx)+" y: "+str(iy-cy))
        #print ("r0="+str(r0)+" r="+str(r))
        
        try:
          alpha=math.asin(r/r0)
        except ValueError:
          #print("out of the circle: ( "+str(int(ix))+" , " +str(int(iy))+" )")
          continue
        
        #print ("alpha: "+str(alpha*180/math.pi))
        r1=r0*alpha
        
        x=r1*math.cos(theta)+len(self._flatten_image)/2
        y=r1*math.sin(theta)+len(self._flatten_image[0])/2
        
        try:
          self._flatten_image[int(x),int(y)]=color
          #print("In range: ( "+str(int(x))+" , " +str(int(y))+" )")
        except IndexError:
         #print("Out of range: ( "+str(int(x))+" , " +str(int(y))+" )")
         continue
    io.imsave("toto.png",self._imgRGB)
    io.imsave("titi.png",self._flatten_image)




  def flattening_color(self,x0,y0):
    cx0=self._cx[0]
    cy0=self._cy[0]
    r0=math.sqrt((x0-cx0)*(x0-cx0)+(y0-cy0)*(y0-cy0))
    theta0=math.atan2(y0-cy0,x0-cx0)
    
    cx=len(self._flatten_image)/2
    cy=len(self._flatten_image[0])/2
    
    #theta=math.atan2(y-cy,x-cx)
    
    x2=cx+r0*math.cos(theta0)
    y2=cx+r0*math.sin(theta0)
    
    
    try:
      color=self._imgRGB[int(x2),int(y2)]
      if (1): # np.linalg.norm(color) > 0.1 ):
        print("( "+str(x0)+" , "+str(y0)+" ) => ( "+str(x2)+" , "+str(y2)+" ) = ("+str(r0)+" , "+str(theta0*180/math.pi)+") = "+str(color))
      return self._imgRGB[int(x2),int(y2)]
    except IndexError:
      return [0,0,0]

  def processFlattenBackward(self):
    self._flatten_image=0*self._imgRGB
    self._flatten_image=rescale(self._flatten_image,1.5, multichannel=True)
    
    for ix in range(len(self._flatten_image)):
      for iy in range(len(self._flatten_image[0])):
        self._flatten_image[ix,iy]=self.flattening_color(ix,iy)
        
    
    io.imsave("toto.png",self._imgRGB)
    io.imsave("titi.png",self._flatten_image)

  
  def find_forward_spherical(self,x,y):
    cx=self._cx[0]
    cy=self._cy[0]
    theta=math.atan2(y-cy,x-cx)
    
    r=math.sqrt((x-cx)*(x-cx)+(y-cy)*(y-cy))
    theta=math.atan2(y-cy,x-cx)
    
    
    return (x,y)
   

  def processFlattenForwardSpherical(self):
    self._flatten_image=0*self._imgRGB
    self._flatten_image=rescale(self._flatten_image,1.5, multichannel=True)
    
    for ix in range(len(self._imgRGB)):
      for iy in range(len(self._imgRGB[0])):
        (theta,phi)=self.find_forward_spherical(ix,iy)
        self._flatten_image[theta,phi]=self._imgRGB[ix,iy]
        
    
    io.imsave("toto.png",self._imgRGB)
    io.imsave("titi.png",self._flatten_image)
    
  # Wikipedia c'est la vie!
  # https://en.wikipedia.org/wiki/Orthographic_map_projection#Mathematics
  # And this one:
  # https://en.wikipedia.org/wiki/Equirectangular_projection

  def flat_backward_spherical_color(self,x0,y0):
    R =self._radii[0]
    cx=self._cx[0]
    cy=self._cy[0]
    
    x=x0-len(self._flatten_image   )/2
    y=y0-len(self._flatten_image[0])/2
    
    
    # First backward compute which coordinates is image point
    lmd0=0
    phi0=0
      
    if (0): # Orthographic
      rho=sqrt(x*x+y*y)
      if (rho==0): rho=.0001 # will be enough
      try:
        c =asin(rho/R)
      except ValueError:
        return [0,0,0]
      
      phi=asin(cos(c)*sin(phi0)+(y*sin(c)*cos(phi0))/rho)
      lmd=lmd0+atan2(x*sin(c),rho*cos(c)*cos(phi0)-y*sin(c)*sin(phi0))
    else: # Equirectangular
      #if (sqrt(x*x+y*y) > R):
      #  return [0,0,0]
     
      lmd=x/(R*cos(phi0))+lmd0
      phi=y/R+phi0
      if (( math.fabs(lmd)> math.pi/2) or ( math.fabs(phi)> math.pi/2)):
        return [0,0,0]
      
    
    # Then forward compute which color is that point on image
    
    # First ensure the point is in the circle
    x1=R*cos(phi)*sin(lmd-lmd0)
    y1=R*(cos(phi0)*sin(phi)-sin(phi0)*cos(phi)*cos(lmd-lmd0))
    if (sqrt(x1*x1+y1*y1) > R):
      return [0,0,0]
    
    # Then add the offest:
    x1=x1+cx
    y1=y1+cy
    
    try:
      color=self._imgRGB[int(x1),int(y1)]
      if (0): # np.linalg.norm(color) > 0.1 ):
        print("( "+str(x0)+" , "+str(y0)+" ) => ( "+str(x2)+" , "+str(y2)+" ) = ("+str(r0)+" , "+str(theta0*180/math.pi)+") = "+str(color))
      return color
    except IndexError:
      return [0,0,0]
    
    

  def processFlattenBackwardSpherical(self):
    print("Clouding.processFlattenBackwardSpherical(): START")
    self._flatten_image=0*self._imgRGB.copy()
    #self._flatten_image=rescale(self._flatten_image,1.5, multichannel=True)
    
    for ix in range(len(self._flatten_image)):
      for iy in range(len(self._flatten_image[0])):
        self._flatten_image[ix,iy]=self.flat_backward_spherical_color(ix,iy)
        
    print("Clouding.processFlattenBackwardSpherical(): END")

    
  def drawFlatten(self):
    fig, ax = plt.subplots(ncols=1, nrows=1, figsize=(10, 4))
    ax.imshow(self._flatten_image, cmap=plt.cm.gray)
    plt.show()


  def writeImage(self,rep):
    fn=os.path.basename(self._fn)
    self.addCircle()
    #io.imsave(os.path.join(rep,"rgb-"+fn),self._imgRGB)
    io.imsave(os.path.join(rep,"circle-"+fn),self._imgRGBcircle)
    io.imsave(os.path.join(rep,"flat-"+fn)  ,self._flatten_image)


def run_thread(fn):
  print("Processing "+fn)
  cl=Clouding(fn,0.5,0.5)
  #cl=Clouding("Happy-Test-Screen-01-825x510s.jpg")
  #cl.fakeprocessCircle()
  cl.processCircle()
  #cl.drawCircle()
  cl.processFlattenBackwardSpherical()
  #cl.drawFlatten()
  cl.writeImage("out")


q = queue.Queue()

def worker(q,i):
    while True:
        item = q.get()
        print(f'Working on {item}')
        run_thread(item)
        print(f'Finished {item}')
        q.task_done()


def main():
  for i in range(8):
    #t=multiprocessing.Process(target=worker, args=(q,i,), daemon=True).start()
    t=Thread(target=worker, args=(q,i,), daemon=True).start()
    
  for f in os.listdir("../../../data/large_processed/"):
    if ("EPIC_" in f):
      f=os.path.join("../../../data/large_processed/", f)
      if os.path.isfile(f):
        #t = Thread(target=worker, args=(f,))
        q.put(f)
  
  q.join()
  


# --------------------------------------------------------------------------
if __name__ == '__main__':
  main()

