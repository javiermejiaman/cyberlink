import { CyberLink } from './cyberlink.js';
import { Point, Circle, Link } from './graphics.js';

let app: App;

let canvas: HTMLCanvasElement;

// control listeners
let ControlOR: HTMLElement;
let ControlXOR: HTMLElement;
let ControlAND: HTMLElement;
let ControlNAND: HTMLElement;
let ControlPlay: HTMLElement;

// tracing table
let tracingTable: HTMLElement[][];

// report
let sparkline: HTMLElement;
let averageError: HTMLElement;
let epoch: HTMLElement;

// training sets
let activeTrainingSet = "OR";

let timer: number;
const refreshRate = 4;

const trainingSets = {
  OR: [
    [0, 0, 0],
    [1, 0, 1],
    [0, 1, 1],
    [1, 1, 1]
  ],
  XOR: [
    [0, 0, 0],
    [1, 0, 1],
    [0, 1, 1],
    [1, 1, 0]
  ],
  AND: [
    [0, 0, 0],
    [1, 0, 0],
    [0, 1, 0],
    [1, 1, 1]
  ],
  NAND: [
    [0, 0, 1],
    [1, 0, 1],
    [0, 1, 1],
    [1, 1, 0]
  ],
};

function initialize() {

  loadDOMElements();
  resizeCanvas();
  app = new App(canvas.getContext('2d')!, window.innerWidth, window.innerHeight, trainingSets.OR);
  deployListeners();

}

function loadDOMElements() {
  
  // get canvas
  canvas = <HTMLCanvasElement> document.getElementById('canvas')!;

  // control listeners
  ControlOR = document.getElementById('or')!;
  ControlXOR = document.getElementById('xor')!;
  ControlAND = document.getElementById('and')!;
  ControlNAND = document.getElementById('nand')!;
  ControlPlay = document.getElementById('play')!;

  // tracing table
  tracingTable = [
    [
      document.getElementById('r0c0')!,
      document.getElementById('r0c1')!,
      document.getElementById('r0c2')!,
      document.getElementById('r0c3')!
    ],
    [
      document.getElementById('r1c0')!,
      document.getElementById('r1c1')!,
      document.getElementById('r1c2')!,
      document.getElementById('r1c3')!
    ],
    [
      document.getElementById('r2c0')!,
      document.getElementById('r2c1')!,
      document.getElementById('r2c2')!,
      document.getElementById('r2c3')!
    ],
    [
      document.getElementById('r3c0')!,
      document.getElementById('r3c1')!,
      document.getElementById('r3c2')!,
      document.getElementById('r3c3')!
    ]
  ];

  // report
  sparkline = document.getElementById('sparkline')!;
  averageError = document.getElementById('average-error')!;
  epoch = document.getElementById('epoch')!;

}

function resizeCanvas() {

  // resize canvas
  canvas.style.width = window.innerWidth + 'px';
  canvas.style.height = window.innerHeight + 'px';
  canvas.width =  window.innerWidth;
  canvas.height = window.innerHeight;

}

function deployListeners() {

  // change training set to OR
  ControlOR.addEventListener('click', () => {
    if(activeTrainingSet != 'OR') {
      removeTrainingSetActiveClass();
      activeTrainingSet = 'OR';
      app.setTrainingSet(trainingSets.OR);
      ControlOR.classList.add('active');
      updateTracingTable();
    }
  });

  // change training set to XOR
  ControlXOR.addEventListener('click', () => {
    if(activeTrainingSet != 'XOR') {
      removeTrainingSetActiveClass();
      activeTrainingSet = 'XOR';
      app.setTrainingSet(trainingSets.XOR);
      ControlXOR.classList.add('active');
      updateTracingTable();
    }
  });

  // change training set to AND
  ControlAND.addEventListener('click', () => {
    if(activeTrainingSet != 'AND') {
      removeTrainingSetActiveClass();
      activeTrainingSet = 'AND';
      app.setTrainingSet(trainingSets.AND);
      ControlAND.classList.add('active');
      updateTracingTable();
    }
  });

  // change training set to NAND
  ControlNAND.addEventListener('click', () => {
    if(activeTrainingSet != 'NAND') {
      removeTrainingSetActiveClass();
      activeTrainingSet = 'NAND';
      app.setTrainingSet(trainingSets.NAND);
      ControlNAND.classList.add('active');
      updateTracingTable();
    }
  });

  // change training set to play or pause the app
  ControlPlay.addEventListener('click', () => {
    if(app.isPlaying()) {
      app.stop();
      clearInterval(timer);
      ControlPlay.children[0].innerHTML ='Iniciar';
      ControlPlay.classList.remove('active');
    } else {
      app.play();
      timer = window.setInterval(update, refreshRate);
      ControlPlay.children[0].innerHTML ='Pausar';
      ControlPlay.classList.add('active');
    }
  });

}

function removeTrainingSetActiveClass() {
  switch(activeTrainingSet) {
    case 'OR':
      ControlOR.classList.remove('active');
    break;
    case 'XOR':
      ControlXOR.classList.remove('active');
    break;
    case 'AND':
      ControlAND.classList.remove('active');
    break;
    case 'NAND':
      ControlNAND.classList.remove('active');
    break;
  }
}

function updateTracingTable() {
  switch(activeTrainingSet) {
    case 'OR':
      for(let i = 0; i < 4; i++) {
        for(let j = 0; j < 3; j++) {
          tracingTable[i][j].innerHTML = trainingSets.OR[i][j].toString();
        }
      }
    break;
    case 'XOR':
      for(let i = 0; i < 4; i++) {
        for(let j = 0; j < 3; j++) {
          tracingTable[i][j].innerHTML = trainingSets.XOR[i][j].toString();
        }
      }
    break;
    case 'AND':
      for(let i = 0; i < 4; i++) {
        for(let j = 0; j < 3; j++) {
          tracingTable[i][j].innerHTML = trainingSets.AND[i][j].toString();
        }
      }
    break;
    case 'NAND':
      for(let i = 0; i < 4; i++) {
        for(let j = 0; j < 3; j++) {
          tracingTable[i][j].innerHTML = trainingSets.NAND[i][j].toString();
        }
      }
    break;
  }
}

function update() {
  let current = app.getCurrent();
  let currentEpoch = app.getEpoch();

  app.next();

  let neurons = app.getNeurons();
  tracingTable[current][3].innerHTML = neurons[6].toFixed(2);

  tracingTable[current][3].className = '';
  switch(activeTrainingSet) {
    case 'OR':
      if(Math.abs(trainingSets.OR[current][2] - neurons[6]) < 0.1) {
        tracingTable[current][3].classList.add('ok');
      } else if(Math.abs(trainingSets.OR[current][2] - neurons[6]) < 0.5) {
        tracingTable[current][3].classList.add('warning');
      } else {
        tracingTable[current][3].classList.add('error');
      }
    break;
    case 'XOR':
      if(Math.abs(trainingSets.XOR[current][2] - neurons[6]) < 0.1) {
        tracingTable[current][3].classList.add('ok');
      } else if(Math.abs(trainingSets.XOR[current][2] - neurons[6]) < 0.5) {
        tracingTable[current][3].classList.add('warning');
      } else {
        tracingTable[current][3].classList.add('error');
      }
    break;
    case 'AND':
      if(Math.abs(trainingSets.AND[current][2] - neurons[6]) < 0.1) {
        tracingTable[current][3].classList.add('ok');
      } else if(Math.abs(trainingSets.AND[current][2] - neurons[6]) < 0.5) {
        tracingTable[current][3].classList.add('warning');
      } else {
        tracingTable[current][3].classList.add('error');
      }
    break;
    case 'NAND':
      if(Math.abs(trainingSets.NAND[current][2] - neurons[6]) < 0.1) {
        tracingTable[current][3].classList.add('ok');
      } else if(Math.abs(trainingSets.NAND[current][2] - neurons[6]) < 0.5) {
        tracingTable[current][3].classList.add('warning');
      } else {
        tracingTable[current][3].classList.add('error');
      }
    break;
  }

  if(currentEpoch % 10 == 0) {
    epoch.innerHTML = currentEpoch.toString();
  }
  
}

class App extends CyberLink {

  // canvas
  private mContext: CanvasRenderingContext2D;
  private mCanvasWidth: number;
  private mCanvasHeight: number;

  // graphics
  private mPoints: Point[];
  private mCircles: Circle[];
  private mLinks: Link[];
  private mAnimate = false;

  constructor(ctx: CanvasRenderingContext2D, canvasWidth: number, canvasHeight: number, trainingSet: number[][], learningRate: number = 0.5, LReLUFactor: number = 0.01) {
    super(trainingSet, learningRate, LReLUFactor);

    // save canvas context
    this.mContext = ctx;

    // save canvas size
    this.mCanvasWidth = canvasWidth;
    this.mCanvasHeight = canvasHeight;
    
    // menu offset and neural network margin
    const offset = 350;
    const margin = ( this.mCanvasWidth * this.mCanvasWidth ) / ( 9 * this.mCanvasHeight );

    // location of neurons and links
    this.mPoints = [
      new Point(offset + margin, this.mCanvasHeight * 0.33),
      new Point(offset + margin, this.mCanvasHeight * 0.67),
      new Point(offset + margin + ( this.mCanvasWidth - offset - 2 * margin ) * 0.5, this.mCanvasHeight * 0.20),
      new Point(offset + margin + ( this.mCanvasWidth - offset - 2 * margin ) * 0.5, this.mCanvasHeight * 0.40),
      new Point(offset + margin + ( this.mCanvasWidth - offset - 2 * margin ) * 0.5, this.mCanvasHeight * 0.60),
      new Point(offset + margin + ( this.mCanvasWidth - offset - 2 * margin ) * 0.5, this.mCanvasHeight * 0.80),
      new Point(this.mCanvasWidth - margin, this.mCanvasHeight * 0.5)
    ];

    // neural network style
    const style = {
      radius: 50,
      thickness: 8,
      positive: '#363645',
      negative: '#b7b7c9',
      input: {
        fill: '#0a99ff',
        stroke: '#2b2b30',
      },
      hidden: {
        fill: '#fcd55c',
        stroke: '#2b2b30',
      },
      output: {
        fill: '#3caea3',
        stroke: '#2b2b30',
      }
    };

    // neurons array
    this.mCircles = [
      new Circle(this.mContext, style.radius, this.mPoints[0], style.input.fill, style.input.stroke, style.thickness),
      new Circle(this.mContext, style.radius, this.mPoints[1], style.input.fill, style.input.stroke, style.thickness),
      new Circle(this.mContext, style.radius, this.mPoints[2], style.hidden.fill, style.hidden.stroke, style.thickness),
      new Circle(this.mContext, style.radius, this.mPoints[3], style.hidden.fill, style.hidden.stroke, style.thickness),
      new Circle(this.mContext, style.radius, this.mPoints[4], style.hidden.fill, style.hidden.stroke, style.thickness),
      new Circle(this.mContext, style.radius, this.mPoints[5], style.hidden.fill, style.hidden.stroke, style.thickness),
      new Circle(this.mContext, style.radius, this.mPoints[6], style.output.fill, style.output.stroke, style.thickness)
    ];

    // links array
    this.mLinks = [
      new Link(this.mContext, super.getWeights()[0], this.mPoints[0], this.mPoints[2], style.positive, style.negative),
      new Link(this.mContext, super.getWeights()[1], this.mPoints[0], this.mPoints[3], style.positive, style.negative),
      new Link(this.mContext, super.getWeights()[2], this.mPoints[0], this.mPoints[4], style.positive, style.negative),
      new Link(this.mContext, super.getWeights()[3], this.mPoints[0], this.mPoints[5], style.positive, style.negative),
      new Link(this.mContext, super.getWeights()[4], this.mPoints[1], this.mPoints[2], style.positive, style.negative),
      new Link(this.mContext, super.getWeights()[5], this.mPoints[1], this.mPoints[3], style.positive, style.negative),
      new Link(this.mContext, super.getWeights()[6], this.mPoints[1], this.mPoints[4], style.positive, style.negative),
      new Link(this.mContext, super.getWeights()[7], this.mPoints[1], this.mPoints[5], style.positive, style.negative),
      new Link(this.mContext, super.getWeights()[8], this.mPoints[2], this.mPoints[6], style.positive, style.negative),
      new Link(this.mContext, super.getWeights()[9], this.mPoints[3], this.mPoints[6], style.positive, style.negative),
      new Link(this.mContext, super.getWeights()[10], this.mPoints[4], this.mPoints[6], style.positive, style.negative),
      new Link(this.mContext, super.getWeights()[11], this.mPoints[5], this.mPoints[6], style.positive, style.negative),
    ];

    this.animate();

  }

  private animate() {
    
    // clear canvas
    this.mContext.clearRect(0, 0, this.mCanvasWidth, this.mCanvasHeight);

    // draw links
    for(let i = this.mLinks.length - 1; i >= 0; i--) {
      this.mLinks[i].setWeight(super.getWeights()[i]);
      this.mLinks[i].recalculate();
      this.mLinks[i].draw();
    }

    // draw neurons
    this.mCircles.forEach((circle) => {
      circle.draw();
    });

    // draw next if animation mode is activated
    if(this.mAnimate) {
      requestAnimationFrame(this.animate.bind(this));
    }

  }

  play() {
    this.mAnimate = true;
    this.animate();
  }

  stop() {
    this.mAnimate = false;
  }

  isPlaying() {
    return this.mAnimate;
  }

}

window.addEventListener('load', () => {initialize()});
