import { Point, Line, Circle, NetworkStyle } from './graphics.js';

// the types on which a neuron can be differentiated
enum NeuronType {
  INPUT,
  HIDDEN,
  OUTPUT
}

// topology of the network
export interface Topology {
  [layer: number]: number;
  length: number;
}

/**
 * Training entry
 * 
 * Input and expected arrays must be in alignment with input and output
 * neurons respectively.
 */
interface TrainingEntry {
  input: number[];
  expected: number[];
}

// dataset on which the neural network will be trained
export interface TrainingSet {
  [trainingEntry: number]: TrainingEntry;
  length: number;
}

/**
 * Neurons of the neural network
 * 
 * Neurons are stateful graphical entities which holds an output value and
 * are uniquely identifiable. Their state is accesible by getters and setters
 * and can be differentiated into a specific type or be linked to other neurons.
 * Linking will generate a Link object referencing both neurons.
 */
export class Neuron extends Circle {

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

    // get indexes
    let originIndex = this.mIndex;
    let targetIndex = neuron.getIndex();

    // get locations
    let originLocation = super.getLocation();
    let targetLocation = neuron.getLocation();

    // validate variables
    if(originIndex != undefined && targetIndex != undefined && originLocation != undefined && targetLocation != undefined) {

      // create link
      let link = new Link(super.getContext(), this.mStyle);

      // link neurons depending on their determined position in the topology
      if(originIndex < targetIndex) {
        link.setLink([this, neuron]);
      } else {
        link.setLink([neuron, this]);
      }

      // set the points upon which the link will be drawn
      link.setStart(originLocation);
      link.setEnd(targetLocation);

      // return link
      return link;

    } else {

      // return false if the validation failed
      return false;

    }
  }

  update() {
    super.draw();
  }

}

/**
 * Links of the network
 * 
 * Links are the most important feature for visualization because their width and
 * color reflects their current weight. Links internally handle a tuple of 2 neurons,
 * the first neuron corresponds to the left most neuron topologically speaking and
 * the last to the right most neuron. Methods getLeft() and getRight() ease the
 * access to this information.
 */
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

/**
 * LAYERS
 * 
 * Layers holds an array of neurons or links, their main purpose is to keep neurons
 * and links structured for their consumption in more complex logic.
 */
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

/**
 * STRUCTURES
 * 
 * Structures stack layers of neurons or links. Their main purpose is to ease the
 * management of neurons and links but also offer utility methods that take advantage
 * of these structures.
 */

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

    // get index
    let index = neuron.getIndex();

    // validate index
    if(index != undefined) {

      let links: Link[] = new Array();

      for (let i = 0; i < this.mLinkLayers.length; i++) {
        for (let j = 0; j < this.mLinkLayers[i].getLinks().length; j++) {
          // get the left neuron of the link
          let current = this.mLinkLayers[i].getLinks()[j].getLeft();
          // validate current neuron
          if(current != undefined) {
            // get index of the current neuron
            let currentIndex = current.getIndex();
            // validate index of the current neuron
            if(currentIndex != undefined) {
              // compare indexes
              if(currentIndex == index) {
                // push link if indexes match
                links.push( this.mLinkLayers[i].getLinks()[j] );
              }
            }
          }
        }
      }

      // return links or false if there are not front links
      if(links.length != 0) {
        return links;
      } else {
        return false;
      }

    } else {
      // return false if index validation failed
      return false;
    }
  }

  rearLinks(neuron: Neuron) {

    // get index
    let index = neuron.getIndex();

    // validate index
    if(index != undefined) {

      let links: Link[] = new Array();

      for (let i = 0; i < this.mLinkLayers.length; i++) {
        for (let j = 0; j < this.mLinkLayers[i].getLinks().length; j++) {
          // get the right neuron of the link
          let current = this.mLinkLayers[i].getLinks()[j].getRight();
          // validate current neuron
          if(current != undefined) {
            // get index of the current neuron
            let currentIndex = current.getIndex();
            // validate index of the current neuron
            if(currentIndex != undefined) {
              // compare indexes
              if(currentIndex == index) {
                // push link if indexes match
                links.push( this.mLinkLayers[i].getLinks()[j] );
              }
            }
          }
        }
      }

      // return links or false if there are not rear links
      if(links.length != 0) {
        return links;
      } else {
        return false;
      }

    } else {

      // return false if the validation failed
      return false;
      
    }
  }

  routes(layer: number, position: number) {

    /**
     * Tracks generation
     * 
     * Tracks array contains each link that is directly or indirectly connected to
     * the root link. The root link is accessible through the parameters layer and
     * position of this function and it is stored in the first layer of this array.
     * The first dimension of tracks stores layers of links and the second dimension
     * stores individual links within a layer.
     */
    let tracks: Link[][] = new Array();

    // initialize first track layer with the root link
    tracks[0] = new Array();
    tracks[0][0] = this.mLinkLayers[layer].getLinks()[position];

    // get remaining track layers
    for (let i = layer + 1; i < this.mLinkLayers.length; i++) {
      // initialize layer
      tracks[i - layer] = new Array();
      for (let j = 0; j < this.mLinkLayers[i].getLinks().length; j++) {
        for (let k = 0; k < tracks[i - layer - 1].length; k++) {
          /**
           * Get right neuron of the previous link and left neuron of the current
           * link to validate that the current link is connected to the root link.
           */
          let rightNeuron = tracks[i - layer - 1][k].getRight();
          let leftNeuron = this.mLinkLayers[i].getLinks()[j].getLeft();
          // validate neurons
          if(rightNeuron != undefined && leftNeuron != undefined) {
            // compare indexes
            if(rightNeuron.getIndex() == leftNeuron.getIndex()) {
              // push link into the layer
              tracks[i - layer].push(this.mLinkLayers[i].getLinks()[j]);
            }
          }
        }
      }
    }

    // routes are are all possible paths that can be generated by permuting the tracks
    let routes: Link[][] = new Array();
    let totalRoutes: number = 1;
    
    // calculate total number of routes
    for (let i = 0; i < tracks.length; i++) {
      totalRoutes *= tracks[i].length;
    }

    // initialize routes array
    for (let i = 0; i < totalRoutes; i++) {
      routes[i] = new Array();
    }

    /**
     * Routes generation
     * 
     * To generate the routes, all paths are calculated pattern-wise by calculating the
     * amount of times a given layer of tracks should be repeated along with repetition
     * of the individual links of that layer.
     */

    // repetition trackers
    let linkRepetition = 1;
    let layerRepetition: number;

    for (let i = tracks.length - 1; i >= 0 ; i--) {

      // calculate layer repetition
      layerRepetition  = totalRoutes / ( linkRepetition * tracks[i].length );

      let counter = 0;
      for (let j = 0; j < layerRepetition; j++) {
        for (let k = 0; k < tracks[i].length; k++) {
          for (let l = 0; l < linkRepetition; l++) {
            routes[counter][tracks.length - i - 1] = tracks[i][k];
            counter++;
          }
        }
      }

      // update link repetition
      linkRepetition *= tracks[i].length;

    }
    
    // return routes or false if there are not routes
    if(routes.length != 0) {
      return routes;
    } else {
      return false;
    }

  }

  resetWeights() {
    for (let i = 0; i < this.mLinkLayers.length; i++) {
      for (let j = 0; j < this.mLinkLayers[i].getLinks().length; j++) {
        this.mLinkLayers[i].getLinks()[j].setWeight(Math.random());
      }
    }
  }

}

/**
 * MAIN LOGIC
 */

export class CyberLink {

  private mContext: CanvasRenderingContext2D;

  private mTopology: Topology;

  private mNeuronStructure: NeuronStructure;
  private mLinkStructure: LinkStructure;

  private mLearningRate: number;

  private mTrainingSet: TrainingSet;
  private mTrainingPointer = 0;

  private mBuffer = 0;
  private mError = 0;

  private mEpoch = 1;

  private mLReLUFactor = 0.01;

  private mActivationFunction = 0;

  constructor(context: CanvasRenderingContext2D, style: NetworkStyle, topology: Topology, learningRate: number, trainingSet: TrainingSet) {

    this.mLearningRate = learningRate;
    this.mTrainingSet = trainingSet;


    this.mTopology = topology;
    this.mNeuronStructure = new NeuronStructure();
    this.mLinkStructure = new LinkStructure();
    this.mContext = context;
    
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
          if(link != false) {
            this.mLinkStructure.getLinkLayers()[i - 1].getLinks().push( link );
          }
        }
      }
    }
  }

  getError() {
    return this.mError;
  }

  getEpoch() {
    return this.mEpoch;
  }

  getTrainingPointer() {
    return this.mTrainingPointer;
  }

  getNeuronStructure() {
    return this.mNeuronStructure;
  }

  getLinkStructure() {
    return this.mLinkStructure;
  }

  setTrainingSet(trainingSet: TrainingSet) {
    this.mTrainingSet = trainingSet;
    this.mTrainingPointer = 0;
    this.mBuffer = 0;
    this.mEpoch = 1;
    this.mLinkStructure.resetWeights();
  }

  setActivation(activationFunction: number) {
    this.mActivationFunction = activationFunction;
    this.mTrainingPointer = 0;
    this.mBuffer = 0;
    this.mEpoch = 1;
    this.mLinkStructure.resetWeights();
  }

  update() {
    this.forwardPropagation();
    for (let i = 0; i < this.mNeuronStructure.getOutputLayer().getNeurons().length; i++) {
      this.mBuffer += this.absoluteError(this.mTrainingSet[this.mTrainingPointer].expected[i], this.mNeuronStructure.getOutputLayer().getNeurons()[i].getOutput());
    }
    this.backPropagation();

    this.mTrainingPointer++;

    if(this.mTrainingPointer >= this.mTrainingSet.length) {

      this.mError = this.mBuffer / ( this.mTrainingSet.length * this.mNeuronStructure.getOutputLayer().getNeurons().length );

      this.mTrainingPointer = 0;
      this.mBuffer = 0;
      this.mEpoch++;

    }

  }

  draw() {

    // clear canvas
    this.mContext.clearRect(0, 0, window.innerWidth, window.innerHeight);

    // draw links
    this.mLinkStructure.getLinkLayers().forEach((linkLayer) => {
      linkLayer.getLinks().forEach((link) => {
        link.update();
      });
    });
        
    // draw neurons
    this.mNeuronStructure.getNeuronLayers().forEach((neuronLayer) => {
      neuronLayer.getNeurons().forEach((neuron) => {
        neuron.update();
      });
    });

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

  private absoluteError(target: number, actual: number) {
    return Math.abs( target - actual );
  }

  private squaredErrorPrime(target: number, actual: number) {
    return - ( target - actual );
  }

  private forwardPropagation() {

    for (let i = 0; i < this.mNeuronStructure.getNeuronLayers()[0].getNeurons().length; i++) {
      this.mNeuronStructure.getInputLayer().getNeurons()[i].setOutput(this.mTrainingSet[this.mTrainingPointer].input[i]);
    }

    for (let i = 0; i < this.mNeuronStructure.getNeuronLayers().length - 1; i++) {
      for (let j = 0; j < this.mNeuronStructure.getNeuronLayers()[i + 1].getNeurons().length; j++) {
        let neuron = this.mNeuronStructure.getNeuronLayers()[i + 1].getNeurons()[j];
        let links = this.mLinkStructure.rearLinks(neuron);
        let feed = 0;

        if(links != false) {
          for (let k = 0; k < links.length; k++) {
            let rearNeuron = links[k].getLeft();
            if(rearNeuron != undefined) {
              feed += links[k].getWeight() * rearNeuron.getOutput();
            }
          }
        }
        switch(this.mActivationFunction) {
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

    let outputNeuronsLength = this.mNeuronStructure.getOutputLayer().getNeurons().length;
    let outputError: number[] = new Array();
    for (let i = 0; i < outputNeuronsLength; i++) {
      outputError[i] = this.squaredErrorPrime(this.mTrainingSet[this.mTrainingPointer].expected[i], this.mNeuronStructure.getOutputLayer().getNeurons()[i].getOutput());
    }

    for (let i = 0; i < this.mLinkStructure.getLinkLayers().length; i++) {
      for (let j = 0; j < this.mLinkStructure.getLinkLayers()[i].getLinks().length; j++) {

        let sum = 0;
        let link = this.mLinkStructure.getLinkLayers()[i].getLinks()[j];
        let routes = this.mLinkStructure.routes(i, j);
        
        let gradient = 0;

        if(routes != false) {
          for (let k = 0; k < routes.length; k++) {
            for (let l = 0; l < routes[k].length; l++) {

              if(l == 0) {
                sum = outputError[k % outputNeuronsLength];
              }

              let neuronRight = routes[k][l].getRight();
              let neuronLeft = routes[k][l].getLeft();
              if(neuronRight != undefined && neuronLeft != undefined) {
                if(l == routes[k].length - 1) {
                  switch(this.mActivationFunction) {
                    case 0:
                      sum *= this.sigmoidPrime(neuronRight.getOutput()) * neuronLeft.getOutput();
                    break;
                    case 1:
                      sum *= this.LReLUPrime(neuronRight.getOutput()) * neuronLeft.getOutput();
                    break;
                  }
                } else {
                  switch(this.mActivationFunction) {
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
        }

        link.setWeight( link.getWeight() - ( this.mLearningRate * gradient ) );

      }
    }
  }

}
