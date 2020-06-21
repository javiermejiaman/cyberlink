export class Point {

  x: number;
  y: number;

  constructor(X: number, Y: number) {
    this.x = X;
    this.y = Y;
  }

}

export class Vector2 {

  x: number;
  y: number;

  constructor(X: number, Y: number) {
    this.x = X;
    this.y = Y;
  }

  magnitude() {
    return Math.sqrt( this.x * this.x + this.y * this.y );
  }

  slope() {
    return ( this.y / this.x );
  }

}

export class Line {

  private mContext: CanvasRenderingContext2D;
  private mThickness: number;
  private mStart: Point;
  private mEnd: Point;
  private mColor: string;

  constructor(ctx: CanvasRenderingContext2D, thickness: number, start: Point, end: Point, color: string = 'black') {
    this.mContext = ctx;
    this.mThickness = thickness;
    this.mStart = start;
    this.mEnd = end;
    this.mColor = color;
  }

  getThickness() {
    return this.mThickness;
  }

  setThickness(thickness: number) {
    this.mThickness = thickness;
  }

  getStart() {
    return this.mStart;
  }

  setStart(start: Point) {
    this.mStart = start;
  }

  getEnd() {
    return this.mEnd;
  }

  setEnd(end: Point) {
    this.mEnd = end;
  }

  getColor() {
    return this.mColor;
  }

  setColor(color: string) {
    this.mColor = color;
  }

  draw() {
    const vector = new Vector2(this.mEnd.x - this.mStart.x, this.mEnd.y - this.mStart.y);
    this.mContext.save();
    this.mContext.translate(this.mStart.x, this.mStart.y - ( this.mThickness / 2 ));
    this.mContext.rotate(Math.atan(vector.slope()));
    this.mContext.beginPath();
    this.mContext.rect(0, 0, vector.magnitude(), this.mThickness);
    this.mContext.fillStyle = this.mColor;
    this.mContext.fill();
    this.mContext.restore();
  }

}

export class Circle {

  private mContext: CanvasRenderingContext2D;
  private mRadius: number;
  private mLocation: Point;
  private mFill: string;
  private mStroke: string;
  private mThickness: number;

  constructor(ctx: CanvasRenderingContext2D, radius: number, location: Point, fill: string = 'black', stroke: string = 'transparent', thickness: number = 0) {
    this.mContext = ctx;
    this.mRadius = radius;
    this.mLocation = location;
    this.mFill = fill;
    this.mStroke = stroke;
    this.mThickness = thickness;
  }

  getRadius() {
    return this.mRadius;
  }

  setRadius(radius: number) {
    this.mRadius = radius;
  }

  getLocation() {
    return this.mLocation;
  }

  setLocation(location: Point) {
    this.mLocation = location;
  }

  getFill() {
    return this.mFill;
  }

  setFill(fill: string) {
    this.mFill = fill;
  }

  getStroke() {
    return this.mStroke;
  }

  setStroke(stroke: string) {
    this.mStroke = stroke;
  }

  getThickness() {
    return this.mFill;
  }

  setThickness(thickness: number) {
    this.mThickness = thickness;
  }

  draw() {
    this.mContext.beginPath();
    this.mContext.arc(this.mLocation.x, this.mLocation.y, this.mRadius, 0, 2 * Math.PI);
    this.mContext.fillStyle = this.mFill;
    this.mContext.fill();
    this.mContext.beginPath();
    this.mContext.arc(this.mLocation.x, this.mLocation.y, this.mRadius, 0, 2 * Math.PI);
    this.mContext.strokeStyle = this.mStroke;
    this.mContext.lineWidth = this.mThickness;
    this.mContext.stroke();
  }

}

export class Link extends Line {

  private mPositive: string;
  private mNegative: string;
  private mWeight: number;

  constructor(ctx: CanvasRenderingContext2D, weight: number, start: Point, end: Point, positive: string, negative: string) {
    super(ctx, 0, start, end);
    this.mWeight = weight;
    this.mPositive = positive;
    this.mNegative = negative;
    this.recalculate();
  }

  getWeight() {
    return this.mWeight;
  }

  setWeight(weight: number) {
    this.mWeight = weight;
    this.recalculate();
  }

  getPositive() {
    return this.mPositive;
  }

  setPositive(positive: string) {
    this.mPositive = positive;
  }

  getNegative() {
    return this.mNegative;
  }

  setNegative(negative: string) {
    this.mNegative = negative;
  }

  private thickness(weight: number) {
    if(weight >= 0) {
      return -14 + ( 30 * Math.exp(weight) ) / ( 1 + Math.exp(weight) );
    } else {
      return -14 + ( 30 * Math.exp(-weight) ) / ( 1 + Math.exp(-weight) );
    }
  }
  
  private color(weight: number) {
    if(weight >= 0) {
      return this.mPositive;
    } else {
      return this.mNegative;
    }
  }

  recalculate() {
    super.setThickness(this.thickness(this.mWeight));
    super.setColor(this.color(this.mWeight));
  }

}