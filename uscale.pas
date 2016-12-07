unit UScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type

  TDoublePoint = record
    X: Double;
    Y: Double;
  end;

  TDoubleRect = record
    case Integer of
      0: (
          Left: Double;
          Top: Double;
          Right: Double;
          Bottom: Double;
        );
      1: (
          TopLeft: TDoublePoint;
          BottomRight: TDoublePoint;
        );
  end;

  TArrayOfTPoint = array of TPoint;


operator + (ADoublePointA, ADoublePointB: TDoublePoint): TDoublePoint;
operator + (APoint: TPoint; ADoublePoint: TDoublePoint): TDoublePoint;
operator + (ADoublePoint: TDoublePoint; AInteger: integer): TDoublePoint;
operator + (APoint: TPoint; AInteger: integer): TPoint;

operator - (ADoublePointA, ADoublePointB: TDoublePoint): TDoublePoint;

operator * (ANumber: Double; ADoublePoint: TDoublePoint): TDoublePoint;
operator * (ADoublePoint: TDoublePoint; ANumber: Double): TDoublePoint;

operator / (ADoublePoint: TDoublePoint; ANumber: Double): TDoublePoint;
operator / (ANumber: Double; ADoublePoint: TDoublePoint): TDoublePoint;

function DoublePoint(AX, AY: Double): TDoublePoint;
function DoubleRect(ATopLeft, ABottomRight: TDoublePoint): TDoubleRect;
function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect;
function WorldPointsToScreen(APoints: array of TDoublePoint): TArrayOfTpoint;
function WorldToScreen(ADoubleRect: TDoubleRect): TRect;
function WorldToScreen(ADoublePoint: TDoublePoint): TPoint;
function WorldToScreen(AVertices: array of TDoublePoint): TArrayOfTpoint;
function ScreenToWorld(AX, AY: Integer): TDoublePoint;
function ScreenToWorldX(AX: Integer): Double;
function ScreenToWorldY(AY: Integer): Double;
procedure ChangeScreenCoords(AX, AY: Double);
procedure SetScreenCoords(AX, AY: Double);
procedure SetScale(AScale: Double; AMidX, AMidY: integer);
procedure UpdateBorderCoords(AX, AY: integer);
procedure UpdateCanvasCoords(ARect: TDoubleRect);
procedure SetCoordsForBars(ACanvasCoords, AImageCoords: TDoubleRect);

const
  BottomRight_Border: integer = 30;
  TopLeft_Border: integer = 10;

var
  Scale: Double = 1.0;
  ScreenCoords: TDoublePoint;
  ImageCoords, CanvasCoords: TDoubleRect;


implementation

  { Operators }

operator + (ADoublePointA, ADoublePointB: TDoublePoint): TDoublePoint;
begin
  Result.X := ADoublePointA.X + ADoublePointB.X;
  Result.Y := ADoublePointA.Y + ADoublePointB.Y;
end;

operator + (APoint: TPoint; ADoublePoint: TDoublePoint): TDoublePoint;
begin
  Result.X := APoint.x + ADoublePoint.X;
  Result.Y := APoint.y + ADoublePoint.Y;
end;

operator + (ADoublePoint: TDoublePoint; AInteger: integer): TDoublePoint;
begin
  Result.X := ADoublePoint.X + AInteger;
  Result.Y := ADoublePoint.Y + AInteger;
end;

operator + (APoint: TPoint; AInteger: integer): TPoint;
begin
  Result.X := APoint.X + AInteger;
  Result.Y := APoint.Y + AInteger;
end;

operator - (ADoublePointA, ADoublePointB: TDoublePoint): TDoublePoint;
begin
  Result.X := ADoublePointA.X - ADoublePointB.X;
  Result.Y := ADoublePointA.Y - ADoublePointB.Y;
end;

operator * (ANumber: Double; ADoublePoint: TDoublePoint): TDoublePoint;
begin
  Result.X := ADoublePoint.X * ANumber;
  Result.Y := ADoublePoint.Y * ANumber;
end;

operator * (ADoublePoint: TDoublePoint; ANumber: Double): TDoublePoint;
begin
  Result.X := ADoublePoint.X * ANumber;
  Result.Y := ADoublePoint.Y * ANumber;
end;

operator / (ADoublePoint: TDoublePoint; ANumber: Double): TDoublePoint;
begin
  Result.X := ADoublePoint.X / ANumber;
  Result.Y := ADoublePoint.Y / ANumber;
end;

operator / (ANumber: Double; ADoublePoint: TDoublePoint): TDoublePoint;
begin
  Result.X := ADoublePoint.X / ANumber;
  Result.Y := ADoublePoint.Y / ANumber;
end;

  { DoublePoint }

function DoublePoint(AX, AY: Double): TDoublePoint;
begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;

  { TDoubleRect }

function DoubleRect(ALeft, ATop, ARight, ABottom: Double): TDoubleRect;
begin
  with Result do begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;

function DoubleRect(ATopLeft, ABottomRight: TDoublePoint): TDoubleRect;
begin
  with Result do begin
    TopLeft := ATopLeft;
    BottomRight := ABottomRight;
  end;
end;

  { ScreenCoords }

procedure SetScreenCoords(AX, AY: Double);
begin
  ScreenCoords := DoublePoint(AX, AY) * Scale;
end;

procedure ChangeScreenCoords(AX, AY: Double);
begin
  ScreenCoords += DoublePoint(AX, AY) * Scale;
end;

procedure SetScale(AScale: Double; AMidX, AMidY: integer);
var
  TempMidCoords: TDoublePoint;
begin
  TempMidCoords := ScreenToWorld(AMidX, AMidY);
  Scale := EnsureRange(AScale, 0.01, 20);
  UScale.ChangeScreenCoords(TempMidCoords.X - ScreenToWorldX(AMidX),
    TempMidCoords.Y - ScreenToWorldY(AMidY));
end;

procedure UpdateBorderCoords(AX, AY: integer);
begin
  if ImageCoords.Left > ScreenToWorldX(AX) then
    ImageCoords.Left := ScreenToWorldX(AX);
  if ImageCoords.Top > ScreenToWorldY(AY) then
    ImageCoords.Top := ScreenToWorldY(AY);
  if ImageCoords.Right < ScreenToWorldX(AX) then
    ImageCoords.Right := ScreenToWorldX(AX);
  if ImageCoords.Bottom < ScreenToWorldY(AY) then
    ImageCoords.Bottom := ScreenToWorldY(AY);
end;

procedure UpdateCanvasCoords(ARect: TDoubleRect);
begin
  if ARect.Left < CanvasCoords.Left then
    CanvasCoords.Left:=ARect.Left;
  if ARect.Top < CanvasCoords.Top then
    CanvasCoords.Top:=ARect.Top;
  if ARect.Right > CanvasCoords.Right then
    CanvasCoords.Right:=ARect.Right;
  if ARect.Bottom > CanvasCoords.Bottom then
    CanvasCoords.Bottom:=ARect.Bottom;
end;

procedure SetCoordsForBars(ACanvasCoords, AImageCoords: TDoubleRect);
begin
  if CanvasCoords.Left > ImageCoords.Left then
    CanvasCoords.Left := ImageCoords.Left - TopLeft_Border;
  if CanvasCoords.Top > ImageCoords.Top then
    CanvasCoords.Top := ImageCoords.Top - TopLeft_Border;
  if CanvasCoords.Right < ImageCoords.Right then
    CanvasCoords.Right := ImageCoords.Right + BottomRight_Border;
  if CanvasCoords.Bottom < ImageCoords.Bottom then
    CanvasCoords.Bottom := ImageCoords.Bottom + BottomRight_Border;
end;

  { Screen -> World}

function ScreenToWorld(AX, AY: Integer): TDoublePoint;
begin
  Result := (ScreenCoords + DoublePoint(AX, AY)) / Scale;
end;

function ScreenToWorldX(AX: Integer): Double;
begin
  Result := (AX + ScreenCoords.X) / Scale;
end;

function ScreenToWorldY(AY: Integer): Double;
begin
  Result := (AY + ScreenCoords.Y) / Scale;
end;

  { World -> Screen }

function WorldToScreen(ADoublePoint: TDoublePoint): TPoint;
begin
  with Result do begin
    x := round(Scale * ADoublePoint.X - ScreenCoords.X);
    y := round(Scale * ADoublePoint.Y - ScreenCoords.Y);
  end;

end;

function WorldToScreen(ADoubleRect: TDoubleRect): TRect;
begin
  with Result do begin
    TopLeft := WorldToScreen(ADoubleRect.TopLeft);
    BottomRight := WorldToScreen(ADoubleRect.BottomRight);
  end;
end;

function WorldToScreen(AVertices: array of TDoublePoint): TArrayOfTpoint;
var
  i: Integer;
begin
  SetLength(Result, Length(AVertices));
  for i := 0 to High(Result) do begin
    Result[i] := WorldToScreen(AVertices[i]);
  end;
end;

function WorldPointsToScreen(APoints: array of TDoublePoint): TArrayOfTpoint;
var
  i: Integer;
  TempPoints: TArrayOfTpoint;
begin
  SetLength(TempPoints, Length(APoints));
  for i := 0 to High(APoints) do begin
    TempPoints[i] := WorldToScreen(APoints[i]);
  end;
  Result := TempPoints;
end;

initialization
SetScreenCoords(0, 0);
ImageCoords.Left := MaxInt;
ImageCoords.Top := MaxInt;
end.

