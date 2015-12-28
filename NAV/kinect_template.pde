import org.openkinect.freenect.*;
import org.openkinect.processing.*;
import ddf.minim.*;
Minim minim;
AudioPlayer Instructions;    // first instructions
AudioPlayer Instruc1;
AudioPlayer Instruc2;
AudioPlayer song0;           // out of limits
AudioPlayer song1;           // target sound
// sounds ==============================================================================
AudioPlayer song2;
AudioPlayer song3;
AudioPlayer song4;
AudioPlayer song5;
AudioPlayer song6;
AudioPlayer song7;
AudioPlayer song8;
AudioPlayer song9;
AudioPlayer song10;
AudioPlayer song11;
AudioPlayer song12;
AudioPlayer song13;
AudioPlayer song14;
AudioPlayer song15;
AudioPlayer song16;
// =====================================================================================

AudioInput input;
PrintWriter output;

PVector location;
PVector velocity;

boolean ActiveSound = true;  // sound activation
boolean boolInstruc = false; // instructions activation

class KinectTracker {
  // Depth threshold
  int threshold = 965;
  // Raw location
  PVector loc;
  // Interpolated location
  PVector lerpedLoc;
  // Depth data
  int[] depth;
  // What we'll show the user
  PImage display;
  KinectTracker() {
    // This is an awkard use of a global variable here
    // But doing it this way for simplicity
    kinect.initDepth();
    kinect.enableMirror(true);
    // Make a blank image
    display = createImage(kinect.width, kinect.height, RGB);
    // Set up the vectors
    loc = new PVector(0, 0);
    lerpedLoc = new PVector(0, 0);
  }
  void track() {
    // Get the raw depth as array of integers
    depth = kinect.getRawDepth();
    // Being overly cautious here
    if (depth == null) return;
    float sumX = 0;
    float sumY = 0;
    float count = 0;
    for (int x = 0; x < kinect.width; x++) {
      for (int y = 0; y < kinect.height; y++) {
        int offset =  x + y*kinect.width;
        // Grabbing the raw depth
        int rawDepth = depth[offset];
        // Testing against threshold
        if (rawDepth < threshold) {
          sumX += x;
          sumY += y;
          count++;
        }
      }
    }
    // As long as we found something
    if (count != 0) {
      loc = new PVector(sumX/count, sumY/count);
    }
    // Interpolating the location, doing it arbitrarily for now
    lerpedLoc.x = PApplet.lerp(lerpedLoc.x, loc.x, 0.3f);
    lerpedLoc.y = PApplet.lerp(lerpedLoc.y, loc.y, 0.3f);
  }
  PVector getLerpedPos() {
    return lerpedLoc;
  }
  PVector getPos() {
    return loc;
  }
  void display() {
    PImage img = kinect.getDepthImage();
    // Being overly cautious here
    if (depth == null || img == null) return;
    // Going to rewrite the depth image to show which pixels are in threshold
    // A lot of this is redundant, but this is just for demonstration purposes
    display.loadPixels();
    for (int x = 0; x < kinect.width; x++) {
      for (int y = 0; y < kinect.height; y++) {
        int offset = x + y * kinect.width;
        // Raw depth
        int rawDepth = depth[offset];
        int pix = x + y * display.width;
        if (rawDepth < threshold) {
          // A red color instead
          display.pixels[pix] = color(150, 50, 50);
        } else {
          display.pixels[pix] = img.pixels[offset];
        }
      }
    }
    display.updatePixels();
    // Draw the image
    image(display, 0, 0);
  }
  int getThreshold() {
    return threshold;
  }
  void setThreshold(int t) {
    threshold =  t;
  }
}

// The kinect stuff is happening in another class
KinectTracker tracker;
Kinect kinect;

void setup() {
  size(640, 520);
  kinect = new Kinect(this);
  tracker = new KinectTracker();
  // sounds
  minim = new Minim(this);
  Instructions = minim.loadFile("Instructions.aiff");
  Instruc1 = minim.loadFile("Instruc1.wav");  
  Instruc2 = minim.loadFile("Instruc2.wav");
  song0 = minim.loadFile("0.aiff"); // out of limits
  song1 = minim.loadFile("1.aiff"); // target sound
  // load sounds =======================================================================
  song2 = minim.loadFile("gato2.wav");
  song3 = minim.loadFile("gato3.wav");
  song4 = minim.loadFile("gato4.wav");
  song5 = minim.loadFile("gato5.wav");
  song6 = minim.loadFile("gato6.wav");
  song7 = minim.loadFile("gato7.wav");
  song8 = minim.loadFile("gato8.wav");
  song9 = minim.loadFile("gato9.wav");
  song10 = minim.loadFile("gato10.wav");
  song11 = minim.loadFile("gato11.wav");
  song12 = minim.loadFile("gato12.wav");
  song13 = minim.loadFile("gato13.wav");
  song14 = minim.loadFile("gato14.wav");
  song15 = minim.loadFile("gato15.wav");
  song16 = minim.loadFile("gato16.wav");
  // ===================================================================================
  // Create the output file in the sketch directory
  output = createWriter("positions.txt"); 
}

void draw() {
  background(255);
  // Run the tracking analysis
  tracker.track();
  // Show the image
  tracker.display();
  // Let's draw the raw location
  PVector v1 = tracker.getPos();
  fill(50, 100, 250, 200);
  noStroke();
  ellipse(v1.x, v1.y, 20, 20);
  // Let's draw the "lerped" location
  PVector v2 = tracker.getLerpedPos();
  fill(100, 250, 50, 200);
  noStroke();
  ellipse(v2.x, v2.y, 20, 20);
  // store position and sounds 
  writePosition(); // write position
  playSound(); // playSound
  // Display some info
  int t = tracker.getThreshold();
  fill(0);
  text("threshold: " + t + "    " +  "framerate: " + int(frameRate) + "    " + 
    "UP increase threshold, DOWN decrease threshold", 10, 500);
}

void writePosition() {
  PVector v1 = tracker.getPos();
  //println(v1.x + " " + v1.y);
  output.println(v1.x + ";" + v1.y); // Write the coordinate to the file
}

void playSound() {
  PVector v1 = tracker.getPos();
  // out of limits 50px
  if (v1.x < 50 || v1.x > 590 || v1.y < 50 || v1.y > 470) {
    if (!song0.isPlaying()) {
      song0.play();
      song0.rewind();
    }
  }
  // here the conditionals 
  // ===================================================================================

  // ===================================================================================
}

// Adjust the threshold with key presses
void keyPressed() {
  int t = tracker.getThreshold();
  if (key == CODED) {
    if (keyCode == UP) {
      t+=5;
      tracker.setThreshold(t);
    } else if (keyCode == DOWN) {
      t-=5;
      tracker.setThreshold(t);
    }
    // search begins
    } else if (keyCode == ' ') {
      println("search begins");
      output.println("search;begins");
      Instructions.play();
      ActiveSound = false;
      if(boolInstruc) {
        Instruc1.play();
        Instruc1.rewind();
      } 
      if(!boolInstruc) {
        boolInstruc = true;
      }
    // search ends
    } else if (key == 'b') {
      println("search ends");
      output.println("search;ends");
      ActiveSound = true;
      Instruc2.play();
      Instruc2.rewind();
    } else {
      output.flush(); // Writes the remaining data to the file
      output.close(); // Finishes the file
      exit(); // Stops the program
  }
}











