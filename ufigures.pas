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
    function IsPointInside(X, Y: integer): Boolean; virtual; abstract;
    function IsIntersects(ABounds: TDoubleRect): Boolean;   virtual; abstract;
  end;

  TPolyLine = class(TFigure)
    Points: array of TDoublePoint;
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
    procedure AddPoint(X, Y: Integer);
    procedure Draw(Canvas: TCanvas); override;
    function IsPointInside(X, Y: integer): Boolean; override;
    function IsIntersects(ABounds: TDoubleRect): Boolean;   override;
  end;

  { TTwoPointsFigure }

  TTwoPointsFigure = class(TFigure)
    Bounds : TDoubleRect;
    procedure AddFirstPoint(X, Y: Integer);
    procedure AddSecondPoint(X, Y: Integer);
  end;

  TRectangle = class(TTwoPointsFigure)
    BrushColor: TColor;
    BrushStyle: TFPBrushStyle;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
    function IsPointInside(X, Y: integer): Boolean; override;
    function IsIntersects(ABounds: TDoubleRect): Boolean;   override;
  end;

  TRoundRect = class(TTwoPointsFigure)
    BrushColor: TColor;
    BrushStyle: TFPBrushStyle;
    RadiusX, RadiusY: integer;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle; ARadiusX, ARadiusY: integer);
    procedure Draw(Canvas: TCanvas); override;
    function IsPointInside(X, Y: integer): Boolean; override;
    function IsIntersects(ABounds: TDoubleRect): Boolean;   override;
  end;

  TEllipse = class(TTwoPointsFigure)
    BrushColor: TColor;
    BrushStyle: TFPBrushStyle;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
    function IsPointInside(X, Y: integer): Boolean; override;
    function IsIntersects(ABounds: TDoubleRect): Boolean;   override;
  end;

  TLine = class(TTwoPointsFigure)
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
    procedure Draw(Canvas: TCanvas); override;
    function IsPointInside(X, Y: integer): Boolean; override;
    function IsIntersects(ABounds: TDoubleRect): Boolean;   override;
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
    function IsPointInside(X, Y: integer): Boolean; override;
    function IsIntersects(ABounds: TDoubleRect): Boolean;   override;
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
  TempPoints: array of TPoint;
begin
  Result := False;
  TempPoints := WorldPointsToScreen(Points);
  for i := Low(TempPoints) to High(TempPoints) do
    if (TempPoints[i].X = X) and (TempPoints[i].Y = Y) then begin
      Result := true;
      Break;
    end;
end;

function TPolyLine.IsIntersects(ABounds: TDoubleRect): Boolean;
var
  i: integer;
  TempPoints: array of TPoint;
  TempBounds: TDoubleRect;
begin
  Result := False;
  TempBounds.Left   := Min(ABounds.Left, ABounds.Right);
  TempBounds.Top    := Min(ABounds.Top, ABounds.Bottom);
  TempBounds.Right  := Max(ABounds.Left, ABounds.Right);
  TempBounds.Bottom := Max(ABounds.Top, ABounds.Bottom);
  TempPoints := WorldPointsToScreen(Points);
  for i := High(TempPoints) downto Low(TempPoints) do
    if (((TempBounds.Left <= TempPoints[i].X) and (TempBounds.Top <= TempPoints[i].Y)) and
       ((TempBounds.Right >= TempPoints[i].X) and (TempBounds.Bottom >= TempPoints[i].Y))) then
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
  if Selected then begin
    Canvas.Pen.Color   := clBlue;
    Canvas.Pen.Width   := Width + 3;
    Canvas.Pen.Style   := psDashDotDot;
    Canvas.Brush.Style := bsDiagCross;
    Canvas.Brush.Color := clBlue;
  end;
  Canvas.Rectangle(WorldToScreen(Bounds));
end;

function TRectangle.IsPointInside(X, Y: integer): Boolean;
var
  i: integer;
  TempBounds: TRect;
begin
  Result := False;
  TempBounds := WorldToScreen(Bounds);
  if ((TempBounds.Left <= X) and (TempBounds.Top <= Y)) and
     ((TempBounds.Right >= X) and (TempBounds.Bottom >= Y)) then begin
    Result := true;
  end;
end;

function TRectangle.IsIntersects(ABounds: TDoubleRect): Boolean;
var
  i: integer;
  RectTempBounds: TRect;
  TempBounds: TDoubleRect;
begin
  Result := False;
  TempBounds.Left   := Min(ABounds.Left, ABounds.Right);
  TempBounds.Top    := Min(ABounds.Top, ABounds.Bottom);
  TempBounds.Right  := Max(ABounds.Left, ABounds.Right);
  TempBounds.Bottom := Max(ABounds.Top, ABounds.Bottom);
  RectTempBounds := WorldToScreen(Bounds);
  if (((TempBounds.Left <= RectTempBounds.Left) and (TempBounds.Top <= RectTempBounds.Top)) and ((TempBounds.Bottom >= RectTempBounds.Left) and (TempBounds.Right >= RectTempBounds.Top))) or
     (((TempBounds.Left <= RectTempBounds.Right) and (TempBounds.Top <= RectTempBounds.Top)) and ((TempBounds.Bottom >= RectTempBounds.Top) and (TempBounds.Right >= RectTempBounds.Right))) or
     (((TempBounds.Left <= RectTempBounds.Left) and (TempBounds.Top <= RectTempBounds.Bottom)) and ((TempBounds.Bottom >= RectTempBounds.Bottom) and (TempBounds.Right >= RectTempBounds.Left))) or
     (((TempBounds.Left <= RectTempBounds.Right) and (TempBounds.Top <= RectTempBounds.Bottom)) and ((TempBounds.Bottom >= RectTempBounds.Right) and (TempBounds.Right >= RectTempBounds.Bottom))) then
       Result := true;
end;

  { TRoundRect }

constructor TRoundRect.Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
   AWidth: integer; ABrushStyle: TFPBrushStyle; ARadiusX, ARadiusY: integer);
begin
  PenColor := APenColor;
  Width := AWidth;
  PenStyle := APenStyle;
  BrushColor:=ABrushColor;
  BrushStyle:=ABrushStyle;
  RadiusX := ARadiusX;
  RadiusY := ARadiusY;
end;

procedure TRoundRect.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color   := PenColor;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Width   := Width;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Pen.Style   := PenStyle;
  if Selected then begin
    Canvas.Pen.Color   := clBlue;
    Canvas.Pen.Width   := Width + 3;
    Canvas.Pen.Style   := psDashDotDot;
    Canvas.Brush.Style := bsDiagCross;
    Canvas.Brush.Color := clBlue;
  end;
  Canvas.RoundRect(WorldToScreen(Bounds), RadiusX, RadiusY);
end;

function TRoundRect.IsPointInside(X, Y: integer): Boolean;
var
  i: integer;
  TempBounds: TRect;
begin
  Result := False;
  TempBounds := WorldToScreen(Bounds);
  if ((TempBounds.Left <= X) and (TempBounds.Top <= Y)) and
     ((TempBounds.Right >= X) and (TempBounds.Bottom >= Y)) then begin
    Result := true;
  end;
end;

function TRoundRect.IsIntersects(ABounds: TDoubleRect): Boolean;
var
  i: integer;
  RectTempBounds: TRect;
  TempBounds: TDoubleRect;
begin
  Result := False;
  TempBounds.Left   := Min(ABounds.Left, ABounds.Right);
  TempBounds.Top    := Min(ABounds.Top, ABounds.Bottom);
  TempBounds.Right  := Max(ABounds.Left, ABounds.Right);
  TempBounds.Bottom := Max(ABounds.Top, ABounds.Bottom);
  RectTempBounds := WorldToScreen(Bounds);
  if (((TempBounds.Left <= RectTempBounds.Left) and (TempBounds.Top <= RectTempBounds.Top)) and ((TempBounds.Bottom >= RectTempBounds.Left) and (TempBounds.Right >= RectTempBounds.Top))) or
     (((TempBounds.Left <= RectTempBounds.Right) and (TempBounds.Top <= RectTempBounds.Top)) and ((TempBounds.Bottom >= RectTempBounds.Top) and (TempBounds.Right >= RectTempBounds.Right))) or
     (((TempBounds.Left <= RectTempBounds.Left) and (TempBounds.Top <= RectTempBounds.Bottom)) and ((TempBounds.Bottom >= RectTempBounds.Bottom) and (TempBounds.Right >= RectTempBounds.Left))) or
     (((TempBounds.Left <= RectTempBounds.Right) and (TempBounds.Top <= RectTempBounds.Bottom)) and ((TempBounds.Bottom >= RectTempBounds.Right) and (TempBounds.Right >= RectTempBounds.Bottom))) then
       Result := true;
end;

  { TEllipse }

constructor TEllipse.Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
  AWidth: integer; ABrushStyle: TFPBrushStyle);
begin
  PenColor := APenColor;
  Width := AWidth;
  PenStyle := APenStyle;
  BrushColor:=ABrushColor;
  BrushStyle:=ABrushStyle;
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color   := PenColor;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Width   := Width;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Pen.Style   := PenStyle;
  if Selected then begin
    Canvas.Pen.Color   := clBlue;
    Canvas.Pen.Width   := Width + 3;
    Canvas.Pen.Style   := psDashDotDot;
    Canvas.Brush.Style := bsDiagCross;
    Canvas.Brush.Color := clBlue;
  end;
  Canvas.Ellipse(WorldToScreen(Bounds));
end;

function TEllipse.IsPointInside(X, Y: integer): Boolean;
var
  i: integer;
  TempBounds: TRect;
begin
  Result := False;
  TempBounds := WorldToScreen(Bounds);
  if ((TempBounds.Left <= X) and (TempBounds.Top <= Y)) and
     ((TempBounds.Right >= X) and (TempBounds.Bottom >= Y)) then begin
    Result := true;
  end;
end;

function TEllipse.IsIntersects(ABounds: TDoubleRect): Boolean;
var
  i: integer;
  RectTempBounds: TRect;
  TempBounds: TDoubleRect;
begin
  Result := False;
  TempBounds.Left   := Min(ABounds.Left, ABounds.Right);
  TempBounds.Top    := Min(ABounds.Top, ABounds.Bottom);
  TempBounds.Right  := Max(ABounds.Left, ABounds.Right);
  TempBounds.Bottom := Max(ABounds.Top, ABounds.Bottom);
  RectTempBounds := WorldToScreen(Bounds);
  if (((TempBounds.Left <= RectTempBounds.Left) and (TempBounds.Top <= RectTempBounds.Top)) and ((TempBounds.Bottom >= RectTempBounds.Left) and (TempBounds.Right >= RectTempBounds.Top))) or
     (((TempBounds.Left <= RectTempBounds.Right) and (TempBounds.Top <= RectTempBounds.Top)) and ((TempBounds.Bottom >= RectTempBounds.Top) and (TempBounds.Right >= RectTempBounds.Right))) or
     (((TempBounds.Left <= RectTempBounds.Left) and (TempBounds.Top <= RectTempBounds.Bottom)) and ((TempBounds.Bottom >= RectTempBounds.Bottom) and (TempBounds.Right >= RectTempBounds.Left))) or
     (((TempBounds.Left <= RectTempBounds.Right) and (TempBounds.Top <= RectTempBounds.Bottom)) and ((TempBounds.Bottom >= RectTempBounds.Right) and (TempBounds.Right >= RectTempBounds.Bottom))) then
       Result := true;
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
  if Selected then begin
    Canvas.Pen.Color   := clBlue;
    Canvas.Pen.Width   := Width + 3;
    Canvas.Pen.Style   := psDashDotDot;
    Canvas.Brush.Style := bsDiagCross;
    Canvas.Brush.Color := clBlue;
  end;
  Canvas.Line(WorldToScreen(Bounds));
end;

function TLine.IsPointInside(X, Y: integer): Boolean;
var
  i: integer;
  TempBounds: TRect;
begin
  Result := False;
  TempBounds := WorldToScreen(Bounds);
  if ((TempBounds.Left <= X) and (TempBounds.Top <= Y)) and
     ((TempBounds.Right >= X) and (TempBounds.Bottom >= Y)) then begin
    Result := true;
  end;
end;

function TLine.IsIntersects(ABounds: TDoubleRect): Boolean;
var
  i: integer;
  RectTempBounds: TRect;
  TempBounds: TDoubleRect;
begin
  Result := False;
  TempBounds.Left   := Min(ABounds.Left, ABounds.Right);
  TempBounds.Top    := Min(ABounds.Top, ABounds.Bottom);
  TempBounds.Right  := Max(ABounds.Left, ABounds.Right);
  TempBounds.Bottom := Max(ABounds.Top, ABounds.Bottom);
  RectTempBounds := WorldToScreen(Bounds);
  if (((TempBounds.Left <= RectTempBounds.Left) and (TempBounds.Top <= RectTempBounds.Top)) and ((TempBounds.Bottom >= RectTempBounds.Left) and (TempBounds.Right >= RectTempBounds.Top))) or
     (((TempBounds.Left <= RectTempBounds.Right) and (TempBounds.Top <= RectTempBounds.Top)) and ((TempBounds.Bottom >= RectTempBounds.Top) and (TempBounds.Right >= RectTempBounds.Right))) or
     (((TempBounds.Left <= RectTempBounds.Left) and (TempBounds.Top <= RectTempBounds.Bottom)) and ((TempBounds.Bottom >= RectTempBounds.Bottom) and (TempBounds.Right >= RectTempBounds.Left))) or
     (((TempBounds.Left <= RectTempBounds.Right) and (TempBounds.Top <= RectTempBounds.Bottom)) and ((TempBounds.Bottom >= RectTempBounds.Right) and (TempBounds.Right >= RectTempBounds.Bottom))) then
       Result := true;
end;

  { TFrame }

procedure TFrame.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style   := psSolid;
  Canvas.Frame(WorldToScreen(Bounds));
end;

  { TPolygon }

constructor TPolygon.Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
  AWidth: integer; ABrushStyle: TFPBrushStyle; ANumberOfAngles: Integer);
begin
  PenColor := APenColor;
  Width := AWidth;
  PenStyle := APenStyle;
  BrushStyle:= ABrushStyle;
  BrushColor:=ABrushColor;
  NumberOfAngles:=ANumberOfAngles;
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
    Canvas.Pen.Color   := clBlue;
    Canvas.Pen.Width   := Width + 3;
    Canvas.Pen.Style   := psDashDotDot;
    Canvas.Brush.Style := bsDiagCross;
    Canvas.Brush.Color := clBlue;
  end;
  begin
    MidlCoord.X := (Bounds.Left + Bounds.Right) / 2;
    MidlCoord.Y := (Bounds.Top + Bounds.Bottom) / 2;
    R := Min(Bounds.Right - MidlCoord.X, Bounds.Bottom - MidlCoord.Y);
    SetLength(Angles, NumberOfAngles);
    for i := 0 to NumberOfAngles - 1 do begin
      Angles[i].x := MidlCoord.X + (R*sin(i * 2 * pi / NumberOfAngles));
      Angles[i].y := MidlCoord.Y + (R*cos(i * 2 * pi / NumberOfAngles));
    end;
  end;
  Canvas.Polygon(WorldToScreen(Angles));
end;

function TPolygon.IsPointInside(X, Y: integer): Boolean;
var
  i: integer;
  TempBounds: TRect;
begin
  Result := False;
  TempBounds := WorldToScreen(Bounds);
  if ((TempBounds.Left <= X) and (TempBounds.Top <= Y)) and
     ((TempBounds.Right >= X) and (TempBounds.Bottom >= Y)) then begin
    Result := true;
  end;
end;

function TPolygon.IsIntersects(ABounds: TDoubleRect): Boolean;
var
  i: integer;
  RectTempBounds: TRect;
  TempBounds: TDoubleRect;
begin
  Result := False;
  TempBounds.Left   := Min(ABounds.Left, ABounds.Right);
  TempBounds.Top    := Min(ABounds.Top, ABounds.Bottom);
  TempBounds.Right  := Max(ABounds.Left, ABounds.Right);
  TempBounds.Bottom := Max(ABounds.Top, ABounds.Bottom);
  RectTempBounds := WorldToScreen(Bounds);
  if (((TempBounds.Left <= RectTempBounds.Left) and (TempBounds.Top <= RectTempBounds.Top)) and ((TempBounds.Bottom >= RectTempBounds.Left) and (TempBounds.Right >= RectTempBounds.Top))) or
     (((TempBounds.Left <= RectTempBounds.Right) and (TempBounds.Top <= RectTempBounds.Top)) and ((TempBounds.Bottom >= RectTempBounds.Top) and (TempBounds.Right >= RectTempBounds.Right))) or
     (((TempBounds.Left <= RectTempBounds.Left) and (TempBounds.Top <= RectTempBounds.Bottom)) and ((TempBounds.Bottom >= RectTempBounds.Bottom) and (TempBounds.Right >= RectTempBounds.Left))) or
     (((TempBounds.Left <= RectTempBounds.Right) and (TempBounds.Top <= RectTempBounds.Bottom)) and ((TempBounds.Bottom >= RectTempBounds.Right) and (TempBounds.Right >= RectTempBounds.Bottom))) then
       Result := true;
end;

end.

