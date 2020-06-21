export class CyberLink {

  // hyperparameters
  private mLearningRate: number;

  // neural network
  private mWeights: number[];
  private mBiases: number[];
  private mNeurons: number[];
  private mLReLUFactor: number;
  private mTrainingSet: number[][];
  private mCurrent: number;
  private mEpoch: number;
  private mIsRunning: boolean;

  constructor(trainintSet: number[][], learningRate: number, LReLUFactor: number) {

    // initialize neural network
    this.mTrainingSet = trainintSet;
    this.mCurrent = 0;
    this.mLearningRate = learningRate;
    this.mLReLUFactor = LReLUFactor;
    this.mEpoch = 0;
    this.mIsRunning = false

    this.mWeights = new Array(12);
    for(let i = 0; i < 12; i++) {
      this.mWeights[i] = Math.random();
    }

    this.mBiases = new Array(5);
    for(let i = 0; i < 5; i++) {
      this.mBiases[i] = Math.random();
    }

    this.mNeurons = new Array(7);
    for(let i = 0; i < 7; i++) {
      this.mNeurons[i] = 0;
    }

  }
  
  next() {
    if(this.mIsRunning) {
      return;
    }

    this.mIsRunning = true;
    this.forwardPropagation();
    this.backPropagation();

    this.mCurrent++;

    if(this.mCurrent >= this.mTrainingSet.length) {
      this.mCurrent = 0;
      this.mEpoch++;
    }
    
    this.mIsRunning = false;
  }

  getWeights() {
    return this.mWeights;
  }

  getNeurons() {
    return this.mNeurons;
  }

  getCurrent() {
    return this.mCurrent;
  }

  getEpoch() {
    return this.mEpoch;
  }

  getError() {
    return this.squaredError(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]);
  }

  getLReLUFactor() {
    return this.mLReLUFactor;
  }

  setLReLUFactor(factor: number) {
    this.mLReLUFactor = factor;
  }

  setLearningRate(learningRate: number) {
    this.mLearningRate = learningRate;
  }

  setTrainingSet(trainingSet: number[][]) {
    this.mTrainingSet = trainingSet;

    for(let i = 0; i < 12; i++) {
      this.mWeights[i] = Math.random();
    }

    for(let i = 0; i < 5; i++) {
      this.mBiases[i] = Math.random();
    }

    this.mCurrent = 0;
    this.mEpoch = 0;
  }

  isRunning() {
    return this.mIsRunning;
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
    // update neurons
    this.mNeurons[0] = this.mTrainingSet[this.mCurrent][0];
    this.mNeurons[1] = this.mTrainingSet[this.mCurrent][1];

    this.mNeurons[2] = this.sigmoid(this.mNeurons[0] * this.mWeights[0] + this.mNeurons[1] * this.mWeights[4] + this.mBiases[0]);
    this.mNeurons[3] = this.sigmoid(this.mNeurons[0] * this.mWeights[1] + this.mNeurons[1] * this.mWeights[5] + this.mBiases[1]);
    this.mNeurons[4] = this.sigmoid(this.mNeurons[0] * this.mWeights[2] + this.mNeurons[1] * this.mWeights[6] + this.mBiases[2]);
    this.mNeurons[5] = this.sigmoid(this.mNeurons[0] * this.mWeights[3] + this.mNeurons[1] * this.mWeights[7] + this.mBiases[3]);

    this.mNeurons[6] = this.sigmoid(this.mNeurons[2] * this.mWeights[8] + this.mNeurons[3] * this.mWeights[9] + this.mNeurons[4] * this.mWeights[10] + this.mNeurons[5] * this.mWeights[11] + this.mBiases[4]);
  }

  private backPropagation() {
    // update weights
    this.mWeights[0] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mWeights[8] * this.sigmoidPrime(this.mNeurons[2]) * this.mNeurons[0];
    this.mWeights[1] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mWeights[9] * this.sigmoidPrime(this.mNeurons[3]) * this.mNeurons[0];
    this.mWeights[2] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mWeights[10] * this.sigmoidPrime(this.mNeurons[4]) * this.mNeurons[0];
    this.mWeights[3] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mWeights[11] * this.sigmoidPrime(this.mNeurons[5]) * this.mNeurons[0];

    this.mWeights[4] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mWeights[8] * this.sigmoidPrime(this.mNeurons[2]) * this.mNeurons[1];
    this.mWeights[5] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mWeights[9] * this.sigmoidPrime(this.mNeurons[3]) * this.mNeurons[1];
    this.mWeights[6] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mWeights[10] * this.sigmoidPrime(this.mNeurons[4]) * this.mNeurons[1];
    this.mWeights[7] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mWeights[11] * this.sigmoidPrime(this.mNeurons[5]) * this.mNeurons[1];

    this.mWeights[8] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mNeurons[2];
    this.mWeights[9] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mNeurons[3];
    this.mWeights[10] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mNeurons[4];
    this.mWeights[11] -= this.mLearningRate * this.squaredErrorPrime(this.mTrainingSet[this.mCurrent][2], this.mNeurons[6]) * this.sigmoidPrime(this.mNeurons[6]) * this.mNeurons[5];
  }

}
