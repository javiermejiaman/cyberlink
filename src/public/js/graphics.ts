/**
 * style
 */
interface CircleStyle {

  radius: number;
  outlineWidth: number;
  fill: string;
  outline: string;

};

interface LineStyle {

  width: number;

  fill: {
    positive: string;
    negative: string;
  }

};

export interface NetworkStyle {

  links: LineStyle;

  neurons: {
    input: CircleStyle;
    hidden: CircleStyle;
    output: CircleStyle;
  }

  margin: {
    top: number;
    right: number;
    bottom: number;
    left: number;
  }

}

/**
 * graphics primitives
 */
export class Point {

  x: number;
  y: number;

  constructor(X: number, Y: number) {
    this.x = X;
    this.y = Y;
  }

}

class Vector2 {

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
  private mWidth: number | undefined;
  private mStart: Point | undefined;
  private mEnd: Point | undefined;
  private mFill: string | undefined;

  constructor(context: CanvasRenderingContext2D, start?: Point, end?: Point, width?: number, fill?: string) {
    this.mContext = context;
    this.mStart = start;
    this.mEnd = end;
    this.mWidth = width;
    this.mFill = fill;
  }

  getContext() {
    return this.mContext;
  }

  setContext(context: CanvasRenderingContext2D) {
    this.mContext = context;
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

  getWidth() {
    return this.mWidth;
  }

  setWidth(width: number) {
    this.mWidth = width;
  }

  getFill() {
    return this.mFill;
  }

  setFill(fill: string) {
    this.mFill = fill;
  }

  draw() {
    if(this.mStart != undefined && this.mEnd != undefined && this.mWidth != undefined && this.mFill != undefined) {
      const vector = new Vector2(this.mEnd.x - this.mStart.x, this.mEnd.y - this.mStart.y);
      this.mContext.save();
      this.mContext.translate(this.mStart.x, this.mStart.y - ( this.mWidth / 2 ));
      this.mContext.rotate(Math.atan(vector.slope()));
      this.mContext.beginPath();
      this.mContext.rect(0, 0, vector.magnitude(), this.mWidth);
      this.mContext.fillStyle = this.mFill;
      this.mContext.fill();
      this.mContext.restore();
    }
  }

}

export class Circle {

  private mContext: CanvasRenderingContext2D;
  private mLocation: Point | undefined;
  private mRadius: number | undefined;
  private mFill: string | undefined;
  private mOutline: string | undefined;
  private mOutlineWidth: number | undefined;

  constructor(context: CanvasRenderingContext2D, location?: Point, radius?: number, fill?: string, outline?: string, outlineWidth?: number) {
    this.mContext = context;
    this.mLocation = location;
    this.mRadius = radius;
    this.mFill = fill;
    this.mOutline = outline;
    this.mOutlineWidth = outlineWidth;
  }

  getContext() {
    return this.mContext;
  }

  setContext(context: CanvasRenderingContext2D) {
    this.mContext = context;
  }

  getLocation() {
    return this.mLocation;
  }

  setLocation(location: Point) {
    this.mLocation = location;
  }

  getRadius() {
    return this.mRadius;
  }

  setRadius(radius: number) {
    this.mRadius = radius;
  }

  getFill() {
    return this.mFill;
  }

  setFill(fill: string) {
    this.mFill = fill;
  }

  getOutline() {
    return this.mOutline;
  }

  setOutline(outline: string) {
    this.mOutline = outline;
  }

  getOutlineWidth() {
    return this.mOutlineWidth;
  }

  setOutlineWidth(outlineWidth: number) {
    this.mOutlineWidth = outlineWidth;
  }

  draw() {
    if(this.mLocation != undefined && this.mRadius != undefined && this.mFill != undefined && this.mOutline != undefined && this.mOutlineWidth != undefined) {
      this.mContext.beginPath();
      this.mContext.arc(this.mLocation.x, this.mLocation.y, this.mRadius, 0, 2 * Math.PI);
      this.mContext.fillStyle = this.mFill;
      this.mContext.fill();
      this.mContext.beginPath();
      this.mContext.arc(this.mLocation.x, this.mLocation.y, this.mRadius, 0, 2 * Math.PI);
      this.mContext.strokeStyle = this.mOutline;
      this.mContext.lineWidth = this.mOutlineWidth;
      this.mContext.stroke();
    }
  }

}
