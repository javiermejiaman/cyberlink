import { CyberLink, TrainingSet, Topology } from './cyberlink.js';
import { SparklineStyle, NetworkStyle } from './graphics.js';

/**
 * configuration
 */

let topology: Topology = [2, 4, 4, 1];

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

const learningRate = 0.5

const trainingSets = [
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

let sparklineStyle: SparklineStyle = {
  fill: '#fff2f3',
  outline: '#e33630'
}

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

  private mUpdateRate = {
    canvas: 4,
    report: 500
  }

  private mCurrentTraining = 0;
  private mTrainingSets: TrainingSet[];

  private mCurrentActivation = 0;

  private mErrorHistory: number[] = new Array();
  private mMaxError = 0;

  private mSparklineContext: CanvasRenderingContext2D;

  constructor(topology: Topology, style: NetworkStyle, learningRate: number, trainingSets: TrainingSet[]) {

    for (let i = 0; i < 50; i++) {
      this.mErrorHistory[i] = 0;
    }

    // get canvas
    this.mCanvas = <HTMLCanvasElement> document.getElementById('canvas')!;
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
    this.mSparklineContext = this.mReport.sparkline.getContext('2d')!;

    this.resizeCanvas();

    this.mCyberLink = new CyberLink(context, style, topology, learningRate, trainingSets[0]);

    window.setInterval(() => {this.update()}, this.mUpdateRate.canvas);

    this.deployListeners();

    this.mTrainingSets = trainingSets;

    this.mCyberLink.draw();

  }

  play() {
    this.mPlay = true;
    this.draw();
  }

  stop() {
    this.mPlay = false;
  }

  private update() {
    if(this.mPlay) {
      
      let trainingPointer = this.mCyberLink.getTrainingPointer();
      let currentEpoch = this.mCyberLink.getEpoch();
  
      this.mCyberLink.update();
  
      let neurons = this.mCyberLink.getNeuronStructure().getOutputLayer().getNeurons();
      this.mTracingTable[trainingPointer][3].innerHTML = neurons[0].getOutput().toFixed(2);
  
      this.mTracingTable[trainingPointer][3].className = '';
  
      switch(this.mCurrentTraining) {
        case 0:
          if(+Math.abs(trainingSets[0][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.1) {
            this.mTracingTable[trainingPointer][3].classList.add('ok');
          } else if(+Math.abs(trainingSets[0][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.5) {
            this.mTracingTable[trainingPointer][3].classList.add('warning');
          } else {
            this.mTracingTable[trainingPointer][3].classList.add('error');
          }
        break;
        case 1:
          if(+Math.abs(trainingSets[1][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.1) {
            this.mTracingTable[trainingPointer][3].classList.add('ok');
          } else if(+Math.abs(trainingSets[1][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.5) {
            this.mTracingTable[trainingPointer][3].classList.add('warning');
          } else {
            this.mTracingTable[trainingPointer][3].classList.add('error');
          }
        break;
        case 2:
          if(+Math.abs(trainingSets[2][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.1) {
            this.mTracingTable[trainingPointer][3].classList.add('ok');
          } else if(+Math.abs(trainingSets[2][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.5) {
            this.mTracingTable[trainingPointer][3].classList.add('warning');
          } else {
            this.mTracingTable[trainingPointer][3].classList.add('error');
          }
        break;
        case 3:
          if(+Math.abs(trainingSets[3][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.1) {
            this.mTracingTable[trainingPointer][3].classList.add('ok');
          } else if(+Math.abs(trainingSets[3][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.5) {
            this.mTracingTable[trainingPointer][3].classList.add('warning');
          } else {
            this.mTracingTable[trainingPointer][3].classList.add('error');
          }
        break;
      }

      if(currentEpoch % 10 == 0) {
        this.mErrorHistory.shift();
        this.mErrorHistory.push(this.mCyberLink.getError());
        this.updateSparkline();
      }

      this.mReport.error.innerHTML = this.mCyberLink.getError().toFixed(5);
  
      if(currentEpoch % 10 == 0) {
        this.mReport.epoch.innerHTML = currentEpoch.toString();
      }
    }
  }

  private draw() {
    if(this.mPlay) {
      this.mCyberLink.draw();
      requestAnimationFrame(this.draw.bind(this));
    }
  }

  private resizeCanvas() {

    this.mCanvas.style.width = window.innerWidth + 'px';
    this.mCanvas.style.height = window.innerHeight + 'px';
    this.mCanvas.width =  window.innerWidth;
    this.mCanvas.height = window.innerHeight;
  
  }

  private deployListeners() {

    // change training set to OR
    this.mButtons.OR.addEventListener('click', () => {
      if(this.mCurrentTraining != 0) {
        this.removeActiveClass();
        this.mCurrentTraining = 0;
        this.mCyberLink.setTrainingSet(trainingSets[0]);
        this.mButtons.OR.classList.add('active');
        this.updateTracingTable();
        this.mMaxError = 0;
      }
    });
  
    // change training set to XOR
    this.mButtons.XOR.addEventListener('click', () => {
      if(this.mCurrentTraining != 1) {
        this.removeActiveClass();
        this.mCurrentTraining = 1;
        this.mCyberLink.setTrainingSet(trainingSets[1]);
        this.mButtons.XOR.classList.add('active');
        this.updateTracingTable();
        this.mMaxError = 0;
      }
    });
  
    // change training set to AND
    this.mButtons.AND.addEventListener('click', () => {
      if(this.mCurrentTraining != 2) {
        this.removeActiveClass();
        this.mCurrentTraining = 2;
        this.mCyberLink.setTrainingSet(trainingSets[2]);
        this.mButtons.AND.classList.add('active');
        this.updateTracingTable();
        this.mMaxError = 0;
      }
    });
  
    // change training set to NAND
    this.mButtons.NAND.addEventListener('click', () => {
      if(this.mCurrentTraining != 3) {
        this.removeActiveClass();
        this.mCurrentTraining = 3;
        this.mCyberLink.setTrainingSet(trainingSets[3]);
        this.mButtons.NAND.classList.add('active');
        this.updateTracingTable();
        this.mMaxError = 0;
      }
    });

    // change activation function to sigmoid
    this.mButtons.SIGMOID.addEventListener('click', () => {
      if(this.mCurrentActivation != 0) {
        this.mButtons.LRELU.classList.remove('active');
        this.mCurrentActivation = 0;
        this.mCyberLink.setActivation(0);
        this.mButtons.SIGMOID.classList.add('active');
        this.mMaxError = 0;
      }
    });

    // change activation function to LReLU
    this.mButtons.LRELU.addEventListener('click', () => {
      if(this.mCurrentActivation != 1) {
        this.mButtons.SIGMOID.classList.remove('active');
        this.mCurrentActivation = 1;
        this.mCyberLink.setActivation(1);
        this.mButtons.LRELU.classList.add('active');
        this.mMaxError = 0;
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

  private updateTracingTable() {
    switch(this.mCurrentTraining) {
      case 0:
        for(let i = 0; i < 4; i++) {
          this.mTracingTable[i][0].innerHTML = this.mTrainingSets[0][i].input[0].toString();
          this.mTracingTable[i][1].innerHTML = this.mTrainingSets[0][i].input[1].toString();
          this.mTracingTable[i][2].innerHTML = this.mTrainingSets[0][i].expected[0].toString();
        }
      break;
      case 1:
        for(let i = 0; i < 4; i++) {
          this.mTracingTable[i][0].innerHTML = this.mTrainingSets[1][i].input[0].toString();
          this.mTracingTable[i][1].innerHTML = this.mTrainingSets[1][i].input[1].toString();
          this.mTracingTable[i][2].innerHTML = this.mTrainingSets[1][i].expected[0].toString();
        }
      break;
      case 2:
        for(let i = 0; i < 4; i++) {
          this.mTracingTable[i][0].innerHTML = this.mTrainingSets[2][i].input[0].toString();
          this.mTracingTable[i][1].innerHTML = this.mTrainingSets[2][i].input[1].toString();
          this.mTracingTable[i][2].innerHTML = this.mTrainingSets[2][i].expected[0].toString();
        }
      break;
      case 3:
        for(let i = 0; i < 4; i++) {
          this.mTracingTable[i][0].innerHTML = this.mTrainingSets[3][i].input[0].toString();
          this.mTracingTable[i][1].innerHTML = this.mTrainingSets[3][i].input[1].toString();
          this.mTracingTable[i][2].innerHTML = this.mTrainingSets[3][i].expected[0].toString();
        }
      break;
    }
  }

  private removeActiveClass() {
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
  }

  private updateSparkline() {

    // recalculate maximum error in the error history
    for (let i = 0; i < 50; i++) {
      if(this.mErrorHistory[i] > this.mMaxError) {
        this.mMaxError = this.mErrorHistory[i];
      }
    }

    // clear the sparkline canvas
    this.mSparklineContext.clearRect(0, 0, 300, 50);

    // verify if max error to avoid division by zero
    if(this.mMaxError != 0) {

      // set sparkline style
      this.mSparklineContext.fillStyle = sparklineStyle.fill;
      this.mSparklineContext.strokeStyle = sparklineStyle.outline;

      // move drawing origin
      this.mSparklineContext.beginPath();
      this.mSparklineContext.moveTo(0, Math.abs(50 - 50 * this.mErrorHistory[0] / this.mMaxError));
      
      // calculate sparkline based on the error history
      for (let i = 0; i < 50; i++) {
        this.mSparklineContext.lineTo((i + 1) * 300 / 50, Math.abs(50 - 50 * this.mErrorHistory[i] / this.mMaxError));
      }
      
      // close the shape and fill
      this.mSparklineContext.lineTo(300, 50);
      this.mSparklineContext.lineTo(0, 50);
      this.mSparklineContext.lineTo(0, Math.abs(50 - 50 * this.mErrorHistory[0] / this.mMaxError));
      this.mSparklineContext.fill();
      
      // repeat procedure for the outline
      this.mSparklineContext.beginPath();
      this.mSparklineContext.moveTo(0, Math.abs(50 - 50 * this.mErrorHistory[0] / this.mMaxError));
      
      for (let i = 0; i < 50; i++) {
        this.mSparklineContext.lineTo((i + 1) * 300 / 50, Math.abs(50 - 50 * this.mErrorHistory[i] / this.mMaxError));
      }
      
      this.mSparklineContext.stroke();

    }
  }

}

// launch app
window.addEventListener('load', () => { new App(topology, networkStyle, learningRate, trainingSets) });
