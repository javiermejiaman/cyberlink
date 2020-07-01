import { Point, Line, Circle, NetworkStyle } from './graphics.js';

enum NeuronType {
  INPUT,
  HIDDEN,
  OUTPUT
}

export interface Topology {
  [index: number]: number;
  length: number;
}

interface TrainingData {
  input: number[];
  expected: number[];
}

export interface TrainingSet {
  [index: number]: TrainingData;
  length: number;
}

class Neuron extends Circle {

  // neuron unique identifier
  private mIndex: number | undefined;

  // neuron data
  private mBias: number;
  private mOutput = 0;

  // style
  private mStyle: NetworkStyle;

  constructor(context: CanvasRenderingContext2D, style: NetworkStyle) {
    super(context);

    // set bias
    this.mBias = Math.random();

    // set style
    this.mStyle = style;
  }

  getIndex() {
    return this.mIndex;
  }

  setIndex(index: number) {
    this.mIndex = index;
  }

  getBias() {
    return this.mBias;
  }

  setBias(bias: number) {
    this.mBias = bias;
  }

  getOutput() {
    return this.mOutput;
  }

  setOutput(output: number) {
    this.mOutput = output;
  }

  differentiate(type: NeuronType) {
    switch(type) {
      case NeuronType.INPUT:
        super.setRadius(this.mStyle.neurons.input.radius);
        super.setFill(this.mStyle.neurons.input.fill);
        super.setOutline(this.mStyle.neurons.input.outline);
        super.setOutlineWidth(this.mStyle.neurons.input.outlineWidth);
      break;
      case NeuronType.HIDDEN:
        super.setRadius(this.mStyle.neurons.hidden.radius);
        super.setFill(this.mStyle.neurons.hidden.fill);
        super.setOutline(this.mStyle.neurons.hidden.outline);
        super.setOutlineWidth(this.mStyle.neurons.hidden.outlineWidth);
      break;
      case NeuronType.OUTPUT:
        super.setRadius(this.mStyle.neurons.output.radius);
        super.setFill(this.mStyle.neurons.output.fill);
        super.setOutline(this.mStyle.neurons.output.outline);
        super.setOutlineWidth(this.mStyle.neurons.output.outlineWidth);
      break;
    }
  }

  link(neuron: Neuron) {

    let originIndex = this.mIndex;
    let targetIndex = neuron.getIndex();

    let originLocation = super.getLocation();
    let targetLocation = neuron.getLocation();

    if(originIndex != undefined && targetIndex != undefined && originLocation != undefined && targetLocation != undefined) {
      let link = new Link(super.getContext(), this.mStyle);

      if(originIndex < targetIndex) {
        link.setLink([this, neuron]);
      } else {
        link.setLink([neuron, this]);
      }

      link.setStart(originLocation);
      link.setEnd(targetLocation);

      return link;

    }
  }

  update() {
    super.draw();
  }

}

class Link extends Line {

  // the indexes of the linked neurons
  private mLink: [Neuron, Neuron] | undefined;

  // link strenght
  private mWeight: number;

  // style
  private mStyle: NetworkStyle;

  constructor(context: CanvasRenderingContext2D, style: NetworkStyle) {
    super(context);

    // set weight
    this.mWeight = Math.random();

    // set style
    this.mStyle = style;
  }

  getLink() {
    return this.mLink;
  }

  setLink(link: [Neuron, Neuron]) {
    this.mLink = link;
  }

  getWeight() {
    return this.mWeight;
  }

  setWeight(weight: number) {
    this.mWeight = weight;
  }

  update() {
    super.setWidth(this.calculateWidth(this.mWeight));
    super.setFill(this.calculateFill(this.mWeight));
    super.draw();
  }

  getLeft() {
    if(this.mLink != undefined) {
      return this.mLink[0];
    }
  }

  getRight() {
    if(this.mLink != undefined) {
      return this.mLink[1];
    }

  }

  private calculateWidth(weight: number) {
    if(weight >= 0) {
      return -(this.mStyle.links.width - 1) + ( 2 * this.mStyle.links.width * Math.exp(weight) ) / ( 1 + Math.exp(weight) );
    } else {
      return -(this.mStyle.links.width - 1) + ( 2 * this.mStyle.links.width * Math.exp(-weight) ) / ( 1 + Math.exp(-weight) );
    }
  }
  
  private calculateFill(weight: number) {
    if(weight >= 0) {
      return this.mStyle.links.fill.positive;
    } else {
      return this.mStyle.links.fill.negative;
    }
  }

}

class NeuronLayer {

  private mNeurons: Neuron[] = new Array();

  getNeurons() {
    return this.mNeurons;
  }

  setNeurons(neurons: Neuron[]) {
    this.mNeurons = neurons;
  }

}

class LinkLayer {

  private mLinks: Link[] = new Array();

  getLinks() {
    return this.mLinks;
  }

  setLinks(links: Link[]) {
    this.mLinks = links;
  }

}

class NeuronStructure {

  private mNeuronLayers: NeuronLayer[] = new Array();

  getNeuronLayers() {
    return this.mNeuronLayers;
  }

  setNeuronLayers(neuronLayers: NeuronLayer[]) {
    this.mNeuronLayers = neuronLayers;
  }

  getInputLayer() {
    return this.getNeuronLayers()[0];
  }

  getOutputLayer() {
    return this.getNeuronLayers()[this.getNeuronLayers().length - 1];
  }

  findNeuron(index: number) {
    for (let i = 0; i < this.mNeuronLayers.length; i++) {
      for (let j = 0; j < this.mNeuronLayers[i].getNeurons().length; j++) {
        if(this.mNeuronLayers[i].getNeurons()[j].getIndex() == index) {
          return this.mNeuronLayers[i].getNeurons()[j];
        }
      }
    }
  }

}

class LinkStructure {

  private mLinkLayers: LinkLayer[] = new Array();

  getLinkLayers() {
    return this.mLinkLayers;
  }

  setLinkLayers(linkLayers: LinkLayer[]) {
    this.mLinkLayers = linkLayers;
  }

  frontLinks(neuron: Neuron) {

    let originIndex = neuron.getIndex();

    if(originIndex != undefined) {
      let links: Link[] = new Array();
      for (let i = 0; i < this.mLinkLayers.length; i++) {
        for (let j = 0; j < this.mLinkLayers[i].getLinks().length; j++) {
          let current = this.mLinkLayers[i].getLinks()[j].getLeft();
          if(current != undefined) {
            let currentIndex = current.getIndex();
            if(currentIndex != undefined) {
              if(currentIndex == originIndex) {
                links.push( this.mLinkLayers[i].getLinks()[j] );
              }
            }
          }
        }
      }
      return links;
    }
  }

  rearLinks(neuron: Neuron) {
    let originIndex = neuron.getIndex();

    if(originIndex != undefined) {
      let links: Link[] = new Array();
      for (let i = 0; i < this.mLinkLayers.length; i++) {
        for (let j = 0; j < this.mLinkLayers[i].getLinks().length; j++) {
          let current = this.mLinkLayers[i].getLinks()[j].getRight();
          if(current != undefined) {
            let currentIndex = current.getIndex();
            if(currentIndex != undefined) {
              if(currentIndex == originIndex) {
                links.push( this.mLinkLayers[i].getLinks()[j] );
              }
            }
          }
        }
      }
      return links;
    }
  }

  routes(layer: number, position: number) {
    let totalRoutes: number = 1;
    let routes: Link[][] = new Array();
    let tracks: Link[][] = new Array();
    tracks[0] = new Array();
    tracks[0][0] = this.mLinkLayers[layer].getLinks()[position];

    for (let i = layer + 1; i < this.mLinkLayers.length; i++) {
      tracks[i - layer] = new Array();
      for (let j = 0; j < this.mLinkLayers[i].getLinks().length; j++) {
        for (let k = 0; k < tracks[i - layer - 1].length; k++) {
          let rightNeuron = tracks[i - layer - 1][k].getRight();
          let leftNeuron = this.mLinkLayers[i].getLinks()[j].getLeft();
          if(rightNeuron != undefined && leftNeuron != undefined) {
            if(rightNeuron.getIndex() == leftNeuron.getIndex()) {
              tracks[i - layer].push(this.mLinkLayers[i].getLinks()[j]);
            }
          }
        }
      }
    }
    
    for (let i = 0; i < tracks.length; i++) {
      totalRoutes *= tracks[i].length;
    }

    for (let i = 0; i < totalRoutes; i++) {
      routes[i] = new Array();
    }

    let cycleSize = 1;
    for (let i = tracks.length - 1; i >= 0 ; i--) {

      let factor = totalRoutes / ( cycleSize * tracks[i].length );

      let counter = 0;
      for (let j = 0; j < factor; j++) {
        for (let k = 0; k < tracks[i].length; k++) {
          for (let l = 0; l < cycleSize; l++) {
            routes[counter][tracks.length - i - 1] = tracks[i][k];
            counter++;
          }
        }
      }

      cycleSize *= tracks[i].length;

    }
    
    return routes;

  }

  resetWeights() {
    for (let i = 0; i < this.mLinkLayers.length; i++) {
      for (let j = 0; j < this.mLinkLayers[i].getLinks().length; j++) {
        this.mLinkLayers[i].getLinks()[j].setWeight(Math.random());
      }
    }
  }

}

export class Network {

  private mContext: CanvasRenderingContext2D;
  private mTopology: Topology;
  private mNeuronStructure: NeuronStructure;
  private mLinkStructure: LinkStructure;
  private mStyle: NetworkStyle;

  constructor(context: CanvasRenderingContext2D, style: NetworkStyle, topology: Topology) {

    this.mTopology = topology;
    this.mNeuronStructure = new NeuronStructure();
    this.mLinkStructure = new LinkStructure();
    this.mContext = context;
    this.mStyle = style;
    
    for( let i = 0; i < this.mTopology.length; i++ ) {
      this.mNeuronStructure.getNeuronLayers()[i] = new NeuronLayer();
      for( let j = 0; j < this.mTopology[i]; j++ ) {
        this.mNeuronStructure.getNeuronLayers()[i].getNeurons()[j] = new Neuron( context, style );
      }
    }

    let index = 0;
    for( let i = 0; i < this.mNeuronStructure.getNeuronLayers().length; i++ ) {
      if( i == 0 ) {
        for( let j = 0; j < this.mNeuronStructure.getNeuronLayers()[i].getNeurons().length; j++ ) {
          this.mNeuronStructure.getNeuronLayers()[i].getNeurons()[j].setIndex( index++ );
          this.mNeuronStructure.getNeuronLayers()[i].getNeurons()[j].setLocation( new Point( style.margin.left, style.margin.top + ( j + 1 ) * ( window.innerHeight - style.margin.bottom ) / ( this.mNeuronStructure.getNeuronLayers()[i].getNeurons().length + 1 ) ) );
          this.mNeuronStructure.getNeuronLayers()[i].getNeurons()[j].differentiate(NeuronType.INPUT);
        }
      } else if( i == this.mNeuronStructure.getNeuronLayers().length - 1 ) {
        for( let j = 0; j < this.mNeuronStructure.getNeuronLayers()[i].getNeurons().length; j++ ) {
          this.mNeuronStructure.getNeuronLayers()[i].getNeurons()[j].setIndex( index++ );
          this.mNeuronStructure.getNeuronLayers()[i].getNeurons()[j].setLocation( new Point( window.innerWidth - style.margin.right, style.margin.top + ( j + 1 ) * ( window.innerHeight - style.margin.bottom ) / ( this.mNeuronStructure.getNeuronLayers()[i].getNeurons().length + 1 ) ) );
          this.mNeuronStructure.getNeuronLayers()[i].getNeurons()[j].differentiate(NeuronType.OUTPUT);
        }
      } else {
        for( let j = 0; j < this.mNeuronStructure.getNeuronLayers()[i].getNeurons().length; j++ ) {
          this.mNeuronStructure.getNeuronLayers()[i].getNeurons()[j].setIndex( index++ );
          this.mNeuronStructure.getNeuronLayers()[i].getNeurons()[j].setLocation( new Point( style.margin.left + i * ( window.innerWidth - style.margin.left - style.margin.right ) / ( this.mNeuronStructure.getNeuronLayers().length - 1 ), style.margin.top + ( j + 1 ) * ( window.innerHeight - style.margin.bottom ) / ( this.mNeuronStructure.getNeuronLayers()[i].getNeurons().length + 1 ) ) );
          this.mNeuronStructure.getNeuronLayers()[i].getNeurons()[j].differentiate(NeuronType.HIDDEN);
        }
      }
    }

    for( let i = 1; i < this.mNeuronStructure.getNeuronLayers().length; i++ ) {
      this.mLinkStructure.getLinkLayers().push( new LinkLayer() );
      for( let j = 0; j < this.mNeuronStructure.getNeuronLayers()[i - 1].getNeurons().length; j++ ) {
        for( let k = 0; k < this.mNeuronStructure.getNeuronLayers()[i].getNeurons().length; k++ ) {
          let link = this.mNeuronStructure.getNeuronLayers()[i - 1].getNeurons()[j].link( this.mNeuronStructure.getNeuronLayers()[i].getNeurons()[k] );
          if(link != undefined) {
            this.mLinkStructure.getLinkLayers()[i - 1].getLinks().push( link );
          }
        }
      }
    }

  }

  getContext() {
    return this.mContext;
  }

  getTopology() {
    return this.mTopology;
  }

  private setTopology(topology: Topology) {
    this.mTopology = topology;
  }

  getNeuronStructure() {
    return this.mNeuronStructure;
  }
  
  private setNeuronStructure(neuronStructure: NeuronStructure) {
    this.mNeuronStructure = neuronStructure;
  }

  getLinkStructure() {
    return this.mLinkStructure;
  }

  private setLinkLayers(linkStructure: LinkStructure) {
    this.mLinkStructure = linkStructure;
  }

}

/**
 * neural network
 */
export class CyberLink {

  // neural network
  private mNetwork: Network;
  private mLearningRate: number;

  private mTrainingSet: TrainingSet;
  private mTrainingPointer = 0;

  private mBuffer = 0;
  private mError = 0;

  private mEpoch = 1;

  private mLReLUFactor = 0.01;

  private mActivation = 0;

  constructor(network: Network, learningRate: number, trainingSet: TrainingSet) {

    this.mNetwork = network;
    this.mLearningRate = learningRate;
    this.mTrainingSet = trainingSet;

  }
  
  update() {
    this.forwardPropagation();
    for (let i = 0; i < this.mNetwork.getNeuronStructure().getOutputLayer().getNeurons().length; i++) {
      this.mBuffer += this.squaredError(this.mTrainingSet[this.mTrainingPointer].expected[i], this.mNetwork.getNeuronStructure().getOutputLayer().getNeurons()[i].getOutput());
    }
    this.backPropagation();

    this.mTrainingPointer++;

    if(this.mTrainingPointer >= this.mTrainingSet.length) {

      this.mError = this.mBuffer / this.mTrainingPointer;

      this.mTrainingPointer = 0;
      this.mBuffer = 0;
      this.mEpoch++;

    }

  }

  draw() {

    // clear canvas
    this.mNetwork.getContext().clearRect(0, 0, window.innerWidth, window.innerHeight);

    // draw links
    this.mNetwork.getLinkStructure().getLinkLayers().forEach((linkLayer) => {
      linkLayer.getLinks().forEach((link) => {
        link.update();
      });
    });
        
    // draw neurons
    this.mNetwork.getNeuronStructure().getNeuronLayers().forEach((neuronLayer) => {
      neuronLayer.getNeurons().forEach((neuron) => {
        neuron.update();
      });
    });

  }

  getEpoch() {
    return this.mEpoch;
  }

  getError() {
    return this.mError;
  }

  getTrainingSet() {
    return this.mTrainingSet;
  }

  setTrainingSet(trainingSet: TrainingSet) {
    this.mTrainingSet = trainingSet;
    this.mTrainingPointer = 0;
    this.mBuffer = 0;
    this.mEpoch = 1;
    this.mNetwork.getLinkStructure().resetWeights();
  }

  setActivation(activation: number) {
    this.mActivation = activation;
    this.mTrainingPointer = 0;
    this.mBuffer = 0;
    this.mEpoch = 1;
    this.mNetwork.getLinkStructure().resetWeights();
  }

  getTrainingPointer() {
    return this.mTrainingPointer;
  }

  getNetwork() {
    return this.mNetwork;
  }

  private LReLU(value: number) {
    return value > 0 ? value : ( value * this.mLReLUFactor );
  }

  private LReLUPrime(value: number) {
    return value > 0 ? 1 : this.mLReLUFactor;
  }

  private sigmoid(value: number) {
    return Math.exp(value) / ( Math.exp(value) + 1);
  }

  private sigmoidPrime(value: number) {
    return value * ( 1 - value );
  }

  private squaredError(target: number, actual: number) {
    return 0.5 * ( ( target - actual ) * (target - actual ) );
  }

  private squaredErrorPrime(target: number, actual: number) {
    return - ( target - actual );
  }

  private forwardPropagation() {

    for (let i = 0; i < this.mNetwork.getNeuronStructure().getNeuronLayers()[0].getNeurons().length; i++) {
      this.mNetwork.getNeuronStructure().getInputLayer().getNeurons()[i].setOutput(this.mTrainingSet[this.mTrainingPointer].input[i]);
    }

    for (let i = 0; i < this.mNetwork.getNeuronStructure().getNeuronLayers().length - 1; i++) {
      for (let j = 0; j < this.mNetwork.getNeuronStructure().getNeuronLayers()[i + 1].getNeurons().length; j++) {
        let neuron = this.mNetwork.getNeuronStructure().getNeuronLayers()[i + 1].getNeurons()[j];
        let links = this.mNetwork.getLinkStructure().rearLinks(neuron);
        let feed = 0;

        if(links != undefined) {
          for (let k = 0; k < links.length; k++) {
            let rearNeuron = links[k].getLeft();
            if(rearNeuron != undefined) {
              feed += links[k].getWeight() * rearNeuron.getOutput();
            }
          }
        }
        switch(this.mActivation) {
          case 0:
            neuron.setOutput(this.sigmoid(feed + neuron.getBias()));
          break;
          case 1:
            neuron.setOutput(this.LReLU(feed + neuron.getBias()));
          break;
        }
      }
    }
  }

  private backPropagation() {

    let outputNeuronsLength = this.mNetwork.getNeuronStructure().getOutputLayer().getNeurons().length;
    let outputError: number[] = new Array();
    for (let i = 0; i < outputNeuronsLength; i++) {
      outputError[i] = this.squaredErrorPrime(this.mTrainingSet[this.mTrainingPointer].expected[i], this.mNetwork.getNeuronStructure().getOutputLayer().getNeurons()[i].getOutput());
    }

    for (let i = 0; i < this.mNetwork.getLinkStructure().getLinkLayers().length; i++) {
      for (let j = 0; j < this.mNetwork.getLinkStructure().getLinkLayers()[i].getLinks().length; j++) {

        let sum = 0;
        let link = this.mNetwork.getLinkStructure().getLinkLayers()[i].getLinks()[j];
        let routes = this.mNetwork.getLinkStructure().routes(i, j);
        
        let gradient = 0;

        for (let k = 0; k < routes.length; k++) {
          for (let l = 0; l < routes[k].length; l++) {

            if(l == 0) {
              sum = outputError[k % outputNeuronsLength];
            }

            let neuronRight = routes[k][l].getRight();
            let neuronLeft = routes[k][l].getLeft();
            if(neuronRight != undefined && neuronLeft != undefined) {
              if(l == routes[k].length - 1) {
                switch(this.mActivation) {
                  case 0:
                    sum *= this.sigmoidPrime(neuronRight.getOutput()) * neuronLeft.getOutput();
                  break;
                  case 1:
                    sum *= this.LReLUPrime(neuronRight.getOutput()) * neuronLeft.getOutput();
                  break;
                }
              } else {
                switch(this.mActivation) {
                  case 0:
                    sum *= this.sigmoidPrime(neuronRight.getOutput()) * routes[k][l].getWeight();
                  break;
                  case 1:
                    sum *= this.LReLUPrime(neuronRight.getOutput()) * routes[k][l].getWeight();
                  break;
                }
              }
            }
          }
          gradient += sum;
        }

        link.setWeight( link.getWeight() - ( this.mLearningRate * gradient ) );

      }
    }
  }

}
