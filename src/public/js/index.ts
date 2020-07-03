import { CyberLink, TrainingSet, Topology, Neuron } from './cyberlink.js';
import { SparklineStyle, NetworkStyle } from './graphics.js';

/**
 * CONFIGURATION
 */

/**
 * Topology of the neural network
 * 
 * Each element in the array represents the number of neurons of
 * a given layer in the network. The first element represents the
 * input layer; the last, the output layer. These layers are
 * implementation dependent, that is, you must modify the implementation
 * to accommodate changes in topology that affect these layers.
 */
let topology: Topology = [2, 4, 4, 1];

/**
 * Appearance of the network
 * 
 * This object describes the colors, sizes and margin rules upon which
 * the network will be styled. Margins are scaled taking into account
 * the number of layers of the network and the aspect ratio of the canvas.
 */
let networkStyle: NetworkStyle = {
  links: {
    width: 15,
    fill: {
      positive: '#363645',
      negative: '#b7b7c9'
    }
  },
  neurons: {
    input: {
      radius: 50,
      outlineWidth: 8,
      fill: '#0a99ff',
      outline: '#2b2b30'
    },
    hidden: {
      radius: 50,
      outlineWidth: 8,
      fill: '#fcd55c',
      outline: '#2b2b30'
    },
    output: {
      radius: 50,
      outlineWidth: 8,
      fill: '#3caea3',
      outline: '#2b2b30'
    }
  },
  margin: {
    top: 0,
    right: 25 + ( window.innerWidth * window.innerWidth ) / ( ( topology.length - 2 ) * 9 * window.innerHeight ),
    bottom: 0,
    left: 350 + ( window.innerWidth * window.innerWidth ) / ( ( topology.length - 2 ) * 9 * window.innerHeight )
  }
}

/**
 * Learning rate of the network
 * 
 * A neural network follows a path towards the correct answer to a question.
 * You can think of the learning rate as how large are the steps taken by
 * the network to get to the answer. A low learning rate can get the network
 * stuck in a local minima, high rates in the other hand, may lead to convergence
 * problems.
 */
const learningRate = 0.5

/**
 * Training sets
 * 
 * This array contains 4 training sets corresponding to basic logic gates table
 * of truth: OR, XOR, AND, NAND correspondingly.
 */
const trainingSets: TrainingSet[] = [
  [
    {
      input: [0, 0],
      expected: [0]
    },
    {
      input: [1, 0],
      expected: [1]
    },
    {
      input: [0, 1],
      expected: [1]
    },
    {
      input: [1, 1],
      expected: [1]
    }
  ],
  [
    {
      input: [0, 0],
      expected: [0]
    },
    {
      input: [1, 0],
      expected: [1]
    },
    {
      input: [0, 1],
      expected: [1]
    },
    {
      input: [1, 1],
      expected: [0]
    }
  ],
  [
    {
      input: [0, 0],
      expected: [0]
    },
    {
      input: [1, 0],
      expected: [0]
    },
    {
      input: [0, 1],
      expected: [0]
    },
    {
      input: [1, 1],
      expected: [1]
    }
  ],
  [
    {
      input: [0, 0],
      expected: [1]
    },
    {
      input: [1, 0],
      expected: [1]
    },
    {
      input: [0, 1],
      expected: [1]
    },
    {
      input: [1, 1],
      expected: [0]
    }
  ]
];

/**
 * Appearance of the sparkline
 */
let sparklineStyle: SparklineStyle = {
  fill: '#fff2f3',
  outline: '#e33630'
}

/**
 * IMPLEMENTATION
 */

class App {

  // app control
  private mPlay = false;

  // app logic
  private mCyberLink: CyberLink;

  // HTML elements
  private mCanvas: HTMLCanvasElement;
  private mButtons: {
    OR: HTMLElement,
    XOR: HTMLElement,
    AND: HTMLElement,
    NAND: HTMLElement,
    SIGMOID: HTMLElement,
    LRELU: HTMLElement,
    PLAY: HTMLElement
  };
  private mTracingTable: HTMLElement[][];
  private mReport: {
    sparkline: HTMLCanvasElement,
    error: HTMLElement,
    epoch: HTMLElement
  }
  private mSparklineContext: CanvasRenderingContext2D;

  // training information and current training set
  private mTrainingSets: TrainingSet[];
  private mCurrentTraining = 0;

  // current activation function
  private mCurrentActivationFunction = 0;

  // near history of the error reported by the network and the maximum entry
  private mErrorHistory: number[] = new Array();
  private mMaxHistoricError = 0;

  // number of milliseconds between neural network updates
  private mUpdateRate = 4;

  constructor(topology: Topology, style: NetworkStyle, learningRate: number, trainingSets: TrainingSet[]) {

    // initialize error history array
    for (let i = 0; i < 50; i++) {
      this.mErrorHistory[i] = 0;
    }

    // canvas
    this.mCanvas = <HTMLCanvasElement> document.getElementById('canvas')!;

    // save canvas context
    let context = this.mCanvas.getContext('2d')!;

    // control listeners
    this.mButtons = {
      OR: document.getElementById('or')!,
      XOR: document.getElementById('xor')!,
      AND: document.getElementById('and')!,
      NAND: document.getElementById('nand')!,
      SIGMOID: document.getElementById('sigmoid')!,
      LRELU: document.getElementById('lrelu')!,
      PLAY: document.getElementById('play')!
    }

    // tracing table
    this.mTracingTable = [
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
    this.mReport = {
      sparkline: <HTMLCanvasElement> document.getElementById('sparkline')!,
      error: document.getElementById('average-error')!,
      epoch: document.getElementById('epoch')!
    }

    // save sparkline context
    this.mSparklineContext = this.mReport.sparkline.getContext('2d')!;

    // resize canvas to fill the viewport
    this.resizeCanvas();

    // main logic
    this.mCyberLink = new CyberLink(context, style, topology, learningRate, trainingSets[0]);

    // neural network update timer
    window.setInterval(() => {this.update()}, this.mUpdateRate);

    // listeners watch for actions and execute task based on them
    this.deployListeners();

    // save training sets
    this.mTrainingSets = trainingSets;

    // draw the just the first frame without activate the play state
    this.mCyberLink.draw();

  }

  play() {
    this.mPlay = true;
    this.draw();
  }

  stop() {
    this.mPlay = false;
  }

  private resizeCanvas() {

    // resize element size along with internal canvas size
    this.mCanvas.style.width = window.innerWidth + 'px';
    this.mCanvas.style.height = window.innerHeight + 'px';
    this.mCanvas.width =  window.innerWidth;
    this.mCanvas.height = window.innerHeight;
  
  }

  private deployListeners() {

    // change training set to OR
    this.mButtons.OR.addEventListener('click', () => {
      if(this.mCurrentTraining != 0) {
        this.updateTrainingSet(0);
      }
    });
  
    // change training set to XOR
    this.mButtons.XOR.addEventListener('click', () => {
      if(this.mCurrentTraining != 1) {
        this.updateTrainingSet(1);
      }
    });
  
    // change training set to AND
    this.mButtons.AND.addEventListener('click', () => {
      if(this.mCurrentTraining != 2) {
        this.updateTrainingSet(2);
      }
    });
  
    // change training set to NAND
    this.mButtons.NAND.addEventListener('click', () => {
      if(this.mCurrentTraining != 3) {
        this.updateTrainingSet(3);
      }
    });

    // change activation function to sigmoid
    this.mButtons.SIGMOID.addEventListener('click', () => {
      if(this.mCurrentActivationFunction != 0) {
        this.mButtons.LRELU.classList.remove('active');
        this.mCurrentActivationFunction = 0;
        this.mCyberLink.setActivation(0);
        this.mButtons.SIGMOID.classList.add('active');
        this.mMaxHistoricError = 0;
      }
    });

    // change activation function to LReLU
    this.mButtons.LRELU.addEventListener('click', () => {
      if(this.mCurrentActivationFunction != 1) {
        this.mButtons.SIGMOID.classList.remove('active');
        this.mCurrentActivationFunction = 1;
        this.mCyberLink.setActivation(1);
        this.mButtons.LRELU.classList.add('active');
        this.mMaxHistoricError = 0;
      }
    });
  
    // play or pause the app
    this.mButtons.PLAY.addEventListener('click', () => {
      if(this.mPlay) {
        this.stop();
        this.mButtons.PLAY.children[0].innerHTML ='Iniciar';
        this.mButtons.PLAY.classList.remove('active');
      } else {
        this.play();
        this.mButtons.PLAY.children[0].innerHTML ='Pausar';
        this.mButtons.PLAY.classList.add('active');
      }
    });
  
  }

  private updateTrainingSet(training: number) {

    // reset maximum historic error
    this.mMaxHistoricError = 0;

    // remove active class from the current active button
    switch(this.mCurrentTraining) {
      case 0:
        this.mButtons.OR.classList.remove('active');
      break;
      case 1:
        this.mButtons.XOR.classList.remove('active');
      break;
      case 2:
        this.mButtons.AND.classList.remove('active');
      break;
      case 3:
        this.mButtons.NAND.classList.remove('active');
      break;
    }

    // add active class to the clicked button
    switch(training) {
      case 0:
        this.mButtons.OR.classList.add('active');
      break;
      case 1:
        this.mButtons.XOR.classList.add('active');
      break;
      case 2:
        this.mButtons.AND.classList.add('active');
      break;
      case 3:
        this.mButtons.NAND.classList.add('active');
      break;
    }

    // update training in class CyberLink and current training reference 
    this.mCyberLink.setTrainingSet(trainingSets[training]);
    this.mCurrentTraining = training;

    // update tracing table
    this.updateTracingTable();

  }

  private updateTracingTable(outputNeurons?: Neuron[], trainingPointer?: number) {

    if(outputNeurons != undefined && trainingPointer != undefined) {

      // update the prediction value in the tracing table
      this.mTracingTable[trainingPointer][3].innerHTML = outputNeurons[0].getOutput().toFixed(2);

      // update the status color of the prediction in the tracing table
      this.mTracingTable[trainingPointer][3].className = '';
      if(parseFloat(Math.abs(trainingSets[this.mCurrentTraining][trainingPointer].expected[0] - outputNeurons[0].getOutput()).toFixed(2)) <= 0.1) {
        this.mTracingTable[trainingPointer][3].classList.add('ok');
      } else if(parseFloat(Math.abs(trainingSets[this.mCurrentTraining][trainingPointer].expected[0] - outputNeurons[0].getOutput()).toFixed(2)) <= 0.5) {
        this.mTracingTable[trainingPointer][3].classList.add('warning');
      } else {
        this.mTracingTable[trainingPointer][3].classList.add('error');
      }

    } else {
      for(let i = 0; i < 4; i++) {

        // update tracing table data
        this.mTracingTable[i][0].innerHTML = this.mTrainingSets[this.mCurrentTraining][i].input[0].toString();
        this.mTracingTable[i][1].innerHTML = this.mTrainingSets[this.mCurrentTraining][i].input[1].toString();
        this.mTracingTable[i][2].innerHTML = this.mTrainingSets[this.mCurrentTraining][i].expected[0].toString();

      }
    }

  }

  private updateSparkline() {

    // recalculate maximum error in the error history
    for (let i = 0; i < 50; i++) {
      if(this.mErrorHistory[i] > this.mMaxHistoricError) {
        this.mMaxHistoricError = this.mErrorHistory[i];
      }
    }

    // clear the sparkline canvas
    this.mSparklineContext.clearRect(0, 0, 300, 50);

    // verify if max error to avoid division by zero
    if(this.mMaxHistoricError != 0) {

      // set sparkline style
      this.mSparklineContext.fillStyle = sparklineStyle.fill;
      this.mSparklineContext.strokeStyle = sparklineStyle.outline;

      // move drawing origin
      this.mSparklineContext.beginPath();
      this.mSparklineContext.moveTo(0, Math.abs(50 - 50 * this.mErrorHistory[0] / this.mMaxHistoricError));
      
      // calculate sparkline based on the error history
      for (let i = 0; i < 50; i++) {
        this.mSparklineContext.lineTo((i + 1) * 300 / 50, Math.abs(50 - 50 * this.mErrorHistory[i] / this.mMaxHistoricError));
      }
      
      // close the shape and fill
      this.mSparklineContext.lineTo(300, 50);
      this.mSparklineContext.lineTo(0, 50);
      this.mSparklineContext.lineTo(0, Math.abs(50 - 50 * this.mErrorHistory[0] / this.mMaxHistoricError));
      this.mSparklineContext.fill();
      
      // repeat procedure for the outline
      this.mSparklineContext.beginPath();
      this.mSparklineContext.moveTo(0, Math.abs(50 - 50 * this.mErrorHistory[0] / this.mMaxHistoricError));
      
      for (let i = 0; i < 50; i++) {
        this.mSparklineContext.lineTo((i + 1) * 300 / 50, Math.abs(50 - 50 * this.mErrorHistory[i] / this.mMaxHistoricError));
      }
      
      this.mSparklineContext.stroke();

    }
  }

  private draw() {
    if(this.mPlay) {
      this.mCyberLink.draw();
      requestAnimationFrame(this.draw.bind(this));
    }
  }

  private update() {
    if(this.mPlay) {
      
      // snapshot before update
      let trainingPointer = this.mCyberLink.getTrainingPointer();
      let currentEpoch = this.mCyberLink.getEpoch();

      // update neural network
      this.mCyberLink.update();

      // snapshot after update
      let outputNeurons = this.mCyberLink.getNeuronStructure().getOutputLayer().getNeurons();

      // update tracing table with last execution data
      this.updateTracingTable(outputNeurons, trainingPointer);

      if(currentEpoch % 10 == 0) {

        // update error history
        this.mErrorHistory.shift();
        this.mErrorHistory.push(this.mCyberLink.getError());

        // update sparkline with the changes
        this.updateSparkline();

        // update average error
        this.mReport.error.innerHTML = this.mCyberLink.getError().toFixed(5);

        // update epoch
        this.mReport.epoch.innerHTML = currentEpoch.toString();
      }
    }
  }

}

// launch app
window.addEventListener('load', () => { new App(topology, networkStyle, learningRate, trainingSets) });
