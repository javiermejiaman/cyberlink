import { CyberLink, TrainingSet, Topology, Network } from './cyberlink.js';
import { NetworkStyle } from './graphics.js';

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

class App {

  // app control
  private mPlay = false;

  // app logic
  private mCyberLink: CyberLink;

  // HTML elements
  private canvas: HTMLCanvasElement;
  private buttons: {
    OR: HTMLElement,
    XOR: HTMLElement,
    AND: HTMLElement,
    NAND: HTMLElement,
    SIGMOID: HTMLElement,
    LRELU: HTMLElement,
    PLAY: HTMLElement
  };
  private tracingTable: HTMLElement[][];
  private report: {
    sparkline: HTMLCanvasElement,
    error: HTMLElement,
    epoch: HTMLElement
  }

  private updateRate = {
    canvas: 4,
    report: 500
  }

  private currentTraining = 0;
  private trainingSets: TrainingSet[];

  private currentActivation = 0;

  constructor(topology: Topology, style: NetworkStyle, learningRate: number, trainingSets: TrainingSet[]) {

    // get canvas
    this.canvas = <HTMLCanvasElement> document.getElementById('canvas')!;
    let context = this.canvas.getContext('2d')!;

    // control listeners
    this.buttons = {
      OR: document.getElementById('or')!,
      XOR: document.getElementById('xor')!,
      AND: document.getElementById('and')!,
      NAND: document.getElementById('nand')!,
      SIGMOID: document.getElementById('sigmoid')!,
      LRELU: document.getElementById('lrelu')!,
      PLAY: document.getElementById('play')!
    }

    // tracing table
    this.tracingTable = [
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
    this.report = {
      sparkline: <HTMLCanvasElement> document.getElementById('sparkline')!,
      error: document.getElementById('average-error')!,
      epoch: document.getElementById('epoch')!
    }

    this.resizeCanvas();

    this.mCyberLink = new CyberLink(new Network(context, style, topology), learningRate, trainingSets[0]);

    window.setInterval(() => {this.update()}, this.updateRate.canvas);

    this.deployListeners();

    this.trainingSets = trainingSets;

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
  
      let neurons = this.mCyberLink.getNetwork().getNeuronStructure().getOutputLayer().getNeurons();
      this.tracingTable[trainingPointer][3].innerHTML = neurons[0].getOutput().toFixed(2);
  
      this.tracingTable[trainingPointer][3].className = '';
  
      switch(this.currentTraining) {
        case 0:
          if(+Math.abs(trainingSets[0][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.1) {
            this.tracingTable[trainingPointer][3].classList.add('ok');
          } else if(+Math.abs(trainingSets[0][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.5) {
            this.tracingTable[trainingPointer][3].classList.add('warning');
          } else {
            this.tracingTable[trainingPointer][3].classList.add('error');
          }
        break;
        case 1:
          if(+Math.abs(trainingSets[1][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.1) {
            this.tracingTable[trainingPointer][3].classList.add('ok');
          } else if(+Math.abs(trainingSets[1][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.5) {
            this.tracingTable[trainingPointer][3].classList.add('warning');
          } else {
            this.tracingTable[trainingPointer][3].classList.add('error');
          }
        break;
        case 2:
          if(+Math.abs(trainingSets[2][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.1) {
            this.tracingTable[trainingPointer][3].classList.add('ok');
          } else if(+Math.abs(trainingSets[2][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.5) {
            this.tracingTable[trainingPointer][3].classList.add('warning');
          } else {
            this.tracingTable[trainingPointer][3].classList.add('error');
          }
        break;
        case 3:
          if(+Math.abs(trainingSets[3][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.1) {
            this.tracingTable[trainingPointer][3].classList.add('ok');
          } else if(+Math.abs(trainingSets[3][trainingPointer].expected[0] - neurons[0].getOutput()).toFixed(2) <= 0.5) {
            this.tracingTable[trainingPointer][3].classList.add('warning');
          } else {
            this.tracingTable[trainingPointer][3].classList.add('error');
          }
        break;
      }

      this.report.error.innerHTML = this.mCyberLink.getError().toFixed(5);
  
      if(currentEpoch % 10 == 0) {
        this.report.epoch.innerHTML = currentEpoch.toString();
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

    this.canvas.style.width = window.innerWidth + 'px';
    this.canvas.style.height = window.innerHeight + 'px';
    this.canvas.width =  window.innerWidth;
    this.canvas.height = window.innerHeight;
  
  }

  private deployListeners() {

    // change training set to OR
    this.buttons.OR.addEventListener('click', () => {
      if(this.currentTraining != 0) {
        this.removeActiveClass();
        this.currentTraining = 0;
        this.mCyberLink.setTrainingSet(trainingSets[0]);
        this.buttons.OR.classList.add('active');
        this.updateTracingTable();
      }
    });
  
    // change training set to XOR
    this.buttons.XOR.addEventListener('click', () => {
      if(this.currentTraining != 1) {
        this.removeActiveClass();
        this.currentTraining = 1;
        this.mCyberLink.setTrainingSet(trainingSets[1]);
        this.buttons.XOR.classList.add('active');
        this.updateTracingTable();
      }
    });
  
    // change training set to AND
    this.buttons.AND.addEventListener('click', () => {
      if(this.currentTraining != 2) {
        this.removeActiveClass();
        this.currentTraining = 2;
        this.mCyberLink.setTrainingSet(trainingSets[2]);
        this.buttons.AND.classList.add('active');
        this.updateTracingTable();
      }
    });
  
    // change training set to NAND
    this.buttons.NAND.addEventListener('click', () => {
      if(this.currentTraining != 3) {
        this.removeActiveClass();
        this.currentTraining = 3;
        this.mCyberLink.setTrainingSet(trainingSets[3]);
        this.buttons.NAND.classList.add('active');
        this.updateTracingTable();
      }
    });

    // change activation function to sigmoid
    this.buttons.SIGMOID.addEventListener('click', () => {
      if(this.currentActivation != 0) {
        this.buttons.LRELU.classList.remove('active');
        this.currentActivation = 0;
        this.mCyberLink.setActivation(0);
        this.buttons.SIGMOID.classList.add('active');
      }
    });

    // change activation function to LReLU
    this.buttons.LRELU.addEventListener('click', () => {
      if(this.currentActivation != 1) {
        this.buttons.SIGMOID.classList.remove('active');
        this.currentActivation = 1;
        this.mCyberLink.setActivation(1);
        this.buttons.LRELU.classList.add('active');
      }
    });
  
    // play or pause the app
    this.buttons.PLAY.addEventListener('click', () => {
      if(this.mPlay) {
        this.stop();
        this.buttons.PLAY.children[0].innerHTML ='Iniciar';
        this.buttons.PLAY.classList.remove('active');
      } else {
        this.play();
        this.buttons.PLAY.children[0].innerHTML ='Pausar';
        this.buttons.PLAY.classList.add('active');
      }
    });
  
  }

  private updateTracingTable() {
    switch(this.currentTraining) {
      case 0:
        for(let i = 0; i < 4; i++) {
          this.tracingTable[i][0].innerHTML = this.trainingSets[0][i].input[0].toString();
          this.tracingTable[i][1].innerHTML = this.trainingSets[0][i].input[1].toString();
          this.tracingTable[i][2].innerHTML = this.trainingSets[0][i].expected[0].toString();
        }
      break;
      case 1:
        for(let i = 0; i < 4; i++) {
          this.tracingTable[i][0].innerHTML = this.trainingSets[1][i].input[0].toString();
          this.tracingTable[i][1].innerHTML = this.trainingSets[1][i].input[1].toString();
          this.tracingTable[i][2].innerHTML = this.trainingSets[1][i].expected[0].toString();
        }
      break;
      case 2:
        for(let i = 0; i < 4; i++) {
          this.tracingTable[i][0].innerHTML = this.trainingSets[2][i].input[0].toString();
          this.tracingTable[i][1].innerHTML = this.trainingSets[2][i].input[1].toString();
          this.tracingTable[i][2].innerHTML = this.trainingSets[2][i].expected[0].toString();
        }
      break;
      case 3:
        for(let i = 0; i < 4; i++) {
          this.tracingTable[i][0].innerHTML = this.trainingSets[3][i].input[0].toString();
          this.tracingTable[i][1].innerHTML = this.trainingSets[3][i].input[1].toString();
          this.tracingTable[i][2].innerHTML = this.trainingSets[3][i].expected[0].toString();
        }
      break;
    }
  }

  private removeActiveClass() {
    switch(this.currentTraining) {
      case 0:
        this.buttons.OR.classList.remove('active');
      break;
      case 1:
        this.buttons.XOR.classList.remove('active');
      break;
      case 2:
        this.buttons.AND.classList.remove('active');
      break;
      case 3:
        this.buttons.NAND.classList.remove('active');
      break;
    }
  }

}

window.addEventListener('load', () => { new App(topology, networkStyle, learningRate, trainingSets) });
