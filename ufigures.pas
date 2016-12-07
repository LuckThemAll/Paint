unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UScale, math, FPCanvas;

Type
  TFigure = class
    PenColor: TColor;
    PenStyle: TFPPenStyle;
    Width: Integer;
    Selected: Boolean;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    function IsPointInside(X, Y: integer): Boolean;         virtual; abstract;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   virtual; abstract;
  end;

  TPolyLine = class(TFigure)
    Points: array of TDoublePoint;
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
    procedure AddPoint(X, Y: Integer);
    procedure Draw(Canvas: TCanvas); override;
    function IsPointInside(X, Y: integer): Boolean; override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
  end;

  { TTwoPointsFigure }

  TTwoPointsFigure = class(TFigure)
    Bounds : TDoubleRect;
    procedure AddFirstPoint(X, Y: Integer);
    procedure AddSecondPoint(X, Y: Integer);
    procedure SetParamsForSelectedFigrs(ACanvas: TCanvas);
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
    function IsPointInside(X, Y: integer): Boolean;         override;
  end;

  TRectangle = class(TTwoPointsFigure)
    BrushColor: TColor;
    BrushStyle: TFPBrushStyle;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
  end;

  TRoundRect = class(TTwoPointsFigure)
    BrushColor: TColor;
    BrushStyle: TFPBrushStyle;
    RadiusX, RadiusY: integer;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle; ARadiusX, ARadiusY: integer);
    procedure Draw(Canvas: TCanvas); override;
  end;

  TEllipse = class(TTwoPointsFigure)
    BrushColor: TColor;
    BrushStyle: TFPBrushStyle;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
  end;

  TLine = class(TTwoPointsFigure)
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
    procedure Draw(Canvas: TCanvas); override;
  end;

  TFrame = class(TTwoPointsFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TPolygon = class(TTwoPointsFigure)
    NumberOfAngles: integer;
    BrushColor: TColor;
    BrushStyle: TFPBrushStyle;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle; ANumberOfAngles: Integer);
    procedure Draw(Canvas: TCanvas); override;
  end;

procedure SaveActualFigure(Figure: TFigure);

var
  Figures: array of TFigure;


implementation

procedure SaveActualFigure(Figure: TFigure);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := Figure;
end;


  { PolyLine }

constructor TPolyLine.Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
begin
  PenColor := APenColor;
  Width := AWidth;
  PenStyle := APenStyle;
end;

procedure TPolyLine.AddPoint(X, Y: Integer);
begin
  SetLength(Points, Length(Points) + 1);
  Points[High(Points)] := ScreenToWorld(X,Y);
  UScale.UpdateBorderCoords(X, Y);
end;

procedure TPolyLine.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color   := PenColor;
  Canvas.Pen.Width   := Width;
  Canvas.Pen.Style   := PenStyle;
  if Selected then begin
    Canvas.Pen.Color   := clBlue;
    Canvas.Pen.Width   := Width + 3;
    Canvas.Pen.Style   := psDashDotDot;
  end;
  Canvas.Polyline(WorldPointsToScreen(Points));
end;

function TPolyLine.IsPointInside(X, Y: integer): Boolean;
var
  i: integer;
  A: array of TPoint;
  DistanceMouseToPoint: integer;
begin
  Result := False;
  A := WorldPointsToScreen(Points);
  for i := Low(A) to High(A) do begin
    DistanceMouseToPoint := round(sqrt((A[i].X - X)**2 + (A[i].Y - Y)**2));
    if DistanceMouseToPoint <= 5 then begin
      Result := true;
      Break;
    end;
  end;
end;

function TPolyLine.IsIntersect(ABounds: TDoubleRect): Boolean;
var
  i: integer;
  TempPoints: array of TPoint;
  TempBounds: TDoubleRect;
  P: TRect;
begin
  Result := False;
  TempBounds.Left   := Min(ABounds.Left, ABounds.Right);
  TempBounds.Top    := Min(ABounds.Top, ABounds.Bottom);
  TempBounds.Right  := Max(ABounds.Left, ABounds.Right);
  TempBounds.Bottom := Max(ABounds.Top, ABounds.Bottom);
  TempPoints := WorldPointsToScreen(Points);
  P := WorldToScreen(TempBounds);
  for i := High(TempPoints) downto Low(TempPoints) do
    if (((P.Left <= TempPoints[i].X) and (P.Top <= TempPoints[i].Y)) and
       ((P.Right >= TempPoints[i].X) and (P.Bottom >= TempPoints[i].Y))) then
      begin
        Result := true;
        Break;
      end;
end;

  { TTwoPointsFigure }

procedure TTwoPointsFigure.AddFirstPoint(X, Y: Integer);
begin
  Bounds := DoubleRect(ScreenToWorldX(X), ScreenToWorldY(Y),
                       ScreenToWorldX(X), ScreenToWorldY(Y));
end;

procedure TTwoPointsFigure.AddSecondPoint(X, Y: Integer);
begin
  Bounds := DoubleRect(Bounds.Left,       Bounds.Top,
                       ScreenToWorldX(X), ScreenToWorldY(Y));
end;

function TTwoPointsFigure.IsIntersect(ABounds: TDoubleRect): Boolean;
var
  TempBounds: TDoubleRect;
begin
  Result := not (
    (min(Bounds.Top,Bounds.Bottom) > max(ABounds.Top,ABounds.Bottom)) or
    (max(Bounds.Top,Bounds.Bottom) < min(ABounds.Top,ABounds.Bottom)) or
    (max(Bounds.Left,Bounds.Right) < min(ABounds.Left,ABounds.Right)) or
    (min(Bounds.Left,Bounds.Right) > max(ABounds.Left,ABounds.Right)));
end;

function TTwoPointsFigure.IsPointInside(X, Y: integer): Boolean;
var
  i: integer;
  A, TempBounds: TRect;
begin
  Result := False;
  A := WorldToScreen(Bounds);
  TempBounds.Left   := Min(A.Left, A.Right);
  TempBounds.Top    := Min(A.Top, A.Bottom);
  TempBounds.Right  := Max(A.Left, A.Right);
  TempBounds.Bottom := Max(A.Top, A.Bottom);
  if ((TempBounds.Left <= X) and (TempBounds.Top <= Y)) and
     ((TempBounds.Right >= X) and (TempBounds.Bottom >= Y)) then begin
    Result := true;
  end;
end;

procedure TTwoPointsFigure.SetParamsForSelectedFigrs(ACanvas: TCanvas);
begin
  ACanvas.Pen.Color   := clBlue;
  ACanvas.Pen.Width   := Width + 3;
  ACanvas.Pen.Style   := psDashDotDot;
  ACanvas.Brush.Style := bsDiagCross;
  ACanvas.Brush.Color := clBlue;
end;

  { TRectangle }

constructor TRectangle.Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
  AWidth: integer; ABrushStyle: TFPBrushStyle);
begin
  PenColor := APenColor;
  Width := AWidth;
  PenStyle := APenStyle;
  BrushColor:=ABrushColor;
  BrushStyle:=ABrushStyle;
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color   := PenColor;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Width   := Width;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Pen.Style   := PenStyle;
  if Selected then
    SetParamsForSelectedFigrs(Canvas);
  Canvas.Rectangle(WorldToScreen(Bounds));
end;

  { TRoundRect }

constructor TRoundRect.Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
   AWidth: integer; ABrushStyle: TFPBrushStyle; ARadiusX, ARadiusY: integer);
begin
  PenColor   := APenColor;
  Width      := AWidth;
  PenStyle   := APenStyle;
  BrushColor := ABrushColor;
  BrushStyle := ABrushStyle;
  RadiusX    := ARadiusX;
  RadiusY    := ARadiusY;
end;

procedure TRoundRect.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color   := PenColor;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Width   := Width;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Pen.Style   := PenStyle;
  if Selected then
    SetParamsForSelectedFigrs(Canvas);
  Canvas.RoundRect(WorldToScreen(Bounds), RadiusX, RadiusY);
end;


  { TEllipse }

constructor TEllipse.Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
  AWidth: integer; ABrushStyle: TFPBrushStyle);
begin
  PenColor   := APenColor;
  Width      := AWidth;
  PenStyle   := APenStyle;
  BrushColor := ABrushColor;
  BrushStyle := ABrushStyle;
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color   := PenColor;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Width   := Width;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Pen.Style   := PenStyle;
  if Selected then
    SetParamsForSelectedFigrs(Canvas);
  Canvas.Ellipse(WorldToScreen(Bounds));
end;

  { TLine }

constructor TLine.Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
begin
  PenColor := APenColor;
  Width := AWidth;
  PenStyle := APenStyle;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color   := PenColor;
  Canvas.Pen.Width   := Width;
  Canvas.Pen.Style   := PenStyle;
  if Selected then
    SetParamsForSelectedFigrs(Canvas);
  Canvas.Line(WorldToScreen(Bounds));
end;

  { TFrame }

procedure TFrame.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psSolid;
  Canvas.Frame(WorldToScreen(Bounds));
end;

  { TPolygon }

constructor TPolygon.Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
  AWidth: integer; ABrushStyle: TFPBrushStyle; ANumberOfAngles: Integer);
begin
  PenColor       := APenColor;
  Width          := AWidth;
  PenStyle       := APenStyle;
  BrushStyle     := ABrushStyle;
  BrushColor     := ABrushColor;
  NumberOfAngles := ANumberOfAngles;
end;

procedure TPolygon.Draw(Canvas: TCanvas);
var
  i: Integer;
  MidlCoord: TDoublePoint;
  R: Double;
  Angles: array of TDoublePoint;
begin
  Canvas.Pen.Color   := PenColor;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Width   := Width;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Pen.Style   := PenStyle;
  if Selected then begin
    SetParamsForSelectedFigrs(Canvas);
  end;
  begin
    MidlCoord.X := (Bounds.Left + Bounds.Right) / 2;
    MidlCoord.Y := (Bounds.Top + Bounds.Bottom) / 2;
    R := Min(abs(Bounds.Right - MidlCoord.X), abs(Bounds.Bottom - MidlCoord.Y));
    SetLength(Angles, NumberOfAngles);
    for i := 0 to NumberOfAngles - 1 do begin
      Angles[i].x := MidlCoord.X + (R*sin(i * 2 * pi / NumberOfAngles));
      Angles[i].y := MidlCoord.Y + (R*cos(i * 2 * pi / NumberOfAngles));
    end;
  end;
  Canvas.Polygon(WorldToScreen(Angles));
end;


end.

