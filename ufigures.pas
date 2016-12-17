unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, windows, Graphics, UScale, math, FPCanvas;

Type

  StrArr = array of String;

  TFigure = class
    FLineColor: TColor;
    FBrushColor: TColor;
    Selected: Boolean;
    Bounds : TDoubleRect;
    function GetBounds: TDoubleRect; virtual;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure Move(ADoublePoint: TDoublePoint); virtual; abstract;
    procedure DrawSelectFrame(ABounds: TDoubleRect; ACanvas: TCanvas); virtual;
    function IsPointInside(ABounds: TDoubleRect): Boolean; virtual; abstract;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   virtual; abstract;
    function AreSegmentsIntersect(A1, A2, B1, B2: TDoublePoint): Boolean;
  end;

  TLinesFigure = class(TFigure)
    FLineStyle: TFPPenStyle;
    FLineWidth: Integer;
  end;

  TPolyLine = class(TLinesFigure)
    Points: array of TDoublePoint;
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
    procedure AddPoint(X, Y: Integer);
    procedure Draw(Canvas: TCanvas); override;
    procedure Move(ADoublePoint: TDoublePoint); override;
    function IsPointInside(ABounds: TDoubleRect): Boolean; override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
end;

  TFilledFigures = class(TLinesFigure)
    FBrushStyle: TFPBrushStyle;
  end;

  { TTwoPointsFigure }

  TTwoPointsFigure = class(TFilledFigures)
    procedure AddFirstPoint(X, Y: Integer);
    procedure AddSecondPoint(X, Y: Integer);
    procedure Move(ADoublePoint: TDoublePoint); override;
    function IsRectIntersectSegment(AFirstpoint, ASecondpoint: TDoublePoint;
      ARect: TDoubleRect): Boolean;
  end;

  TRectangle = class(TTwoPointsFigure)
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
    function IsPointInside(ABounds: TDoubleRect): Boolean; override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
  end;

  TRoundRect = class(TTwoPointsFigure)
    RadiusX, RadiusY: integer;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle; ARadiusX, ARadiusY: integer);
    procedure Draw(Canvas: TCanvas); override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
    function IsPointInside(ABounds: TDoubleRect): Boolean; override;
  end;

  TEllipse = class(TTwoPointsFigure)
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
    function IsPointInside(ABounds: TDoubleRect): Boolean; override;
  end;

  TLine = class(TTwoPointsFigure)
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
    procedure Draw(Canvas: TCanvas); override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
    function IsPointInside(ABounds: TDoubleRect): Boolean; override;
  end;

  TFrame = class(TTwoPointsFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TPolygon = class(TTwoPointsFigure)
    NumberOfAngles: integer;
    Angles: array of TDoublePoint;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle; ANumberOfAngles: Integer);
    procedure Draw(Canvas: TCanvas); override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
    function IsPointInside(ABounds: TDoubleRect): Boolean; override;
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

  { TFigure }

function TFigure.AreSegmentsIntersect(A1, A2, B1, B2: TDoublePoint): Boolean;
var
  V1,V2,V3,V4, Ax1, Ay1, Ax2, Ay2, Bx1, By1, Bx2, By2: Double;
begin
  Ax1 := A1.X;
  Ay1 := A1.Y;
  Ax2 := A2.X;
  Ay2 := A2.Y;
  Bx1 := B1.X;
  By1 := B1.Y;
  Bx2 := B2.X;
  By2 := B2.Y;
  V1 := (Bx2 - Bx1) * (Ay1 - By1) - (By2 - By1) * (Ax1 - Bx1);
  V2 := (Bx2 - Bx1) * (Ay2 - By1) - (By2 - By1) * (Ax2 - Bx1);
  V3 := (Ax2 - Ax1) * (By1 - Ay1) - (Ay2 - Ay1) * (Bx1 - Ax1);
  V4 := (Ax2 - Ax1) * (By2 - Ay1) - (Ay2 - Ay1) * (Bx2 - Ax1);
  Result := (V1 * V2 < 0) and (V3 * V4 < 0);
end;

function TFigure.GetBounds: TDoubleRect;
begin
  with Result do begin
    Top := Min(Bounds.Top, Bounds.Bottom);
    Left := Min(Bounds.Left, Bounds.Right);
    Bottom := Max(Bounds.Top, Bounds.Bottom);
    Right := Max(Bounds.Left, Bounds.Right);
  end;
end;

procedure TFigure.DrawSelectFrame(ABounds: TDoubleRect; ACanvas: TCanvas);
var
  Delta: integer = 5;
begin
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psDash;
  ABounds.Top    -=Delta;
  ABounds.Left   -=Delta;
  ABounds.Bottom +=Delta;
  ABounds.Right  +=Delta;
  ACanvas.Frame(WorldToScreen(ABounds));
end;

  { PolyLine }

constructor TPolyLine.Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
begin
  FLineColor := APenColor;
  FLineWidth := AWidth;
  FLineStyle := APenStyle;
end;

procedure TPolyLine.AddPoint(X, Y: Integer);
var
  WorldX, WorldY: Double;
begin
  WorldX:= ScreenToWorld(X, Y).X;
  WorldY:= ScreenToWorld(X, Y).Y;
  SetLength(Points, Length(Points) + 1);
  Points[High(Points)] := ScreenToWorld(X,Y);
  If WorldX < Bounds.Left then
    Bounds.Left := WorldX;
  If WorldY < Bounds.Top then
    Bounds.Top := WorldY;
  If WorldX > bounds.Right then
    Bounds.Right := WorldX;
  If WorldY > Bounds.Bottom then
    Bounds.Bottom := WorldY;
end;

procedure TPolyLine.Draw(Canvas: TCanvas);
begin
  if Selected then begin
    DrawSelectFrame(Bounds, Canvas);
  end;
  Canvas.Pen.Color   := FLineColor;
  Canvas.Pen.Width   := FLineWidth;
  Canvas.Pen.Style   := FLineStyle;
  Canvas.Polyline(WorldPointsToScreen(Points));
end;

function TPolyLine.IsPointInside(ABounds: TDoubleRect): Boolean;
var
  i: integer;
  A: array of TPoint;
  DistanceMouseToPoint: integer;
  B: TRect;
begin
  Result := False;
  A := WorldPointsToScreen(Points);
  B := WorldToScreen(ABounds);
  for i := Low(A) to High(A) do begin
    DistanceMouseToPoint := round(sqrt((A[i].X - B.Left)**2 + (A[i].Y - B.Top)**2));
    if DistanceMouseToPoint <= ((FLineWidth div 2) + 1) then begin
      Result := true;
      Break;
    end;
  end;
end;

function TPolyLine.IsIntersect(ABounds: TDoubleRect): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := Low(Points) to High(Points)-1 do
    with ABounds do begin
      if AreSegmentsIntersect(Points[i], Points[i+1], DoublePoint(Left, Bottom), TopLeft) or
         AreSegmentsIntersect(Points[i], Points[i+1], TopLeft, DoublePoint(Right, Top)) or
         AreSegmentsIntersect(Points[i], Points[i+1], DoublePoint(Right, Top), BottomRight) or
         AreSegmentsIntersect(Points[i], Points[i+1], BottomRight, DoublePoint(Left, Bottom)) or
         ((Points[i].Y <= max(Top, round(Bottom))) and (Points[i].Y >= min(Top, Bottom)) and
         (Points[i].X <= max(Left, Right)) and (Points[i].X >= min(Left, Right)))
      then Result := True;
    end;
end;

procedure TPolyLine.Move(ADoublePoint: TDoublePoint);
var
  i: integer;
begin
  for i := Low(Points) to High(Points) do begin
    Points[i] += ADoublePoint;
  end;
  with Bounds do begin
      Top    += ADoublePoint.Y;
      Left   += ADoublePoint.X;
      Bottom += ADoublePoint.Y;
      Right  += ADoublePoint.X;
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

function TTwoPointsFigure.IsRectIntersectSegment(AFirstpoint, ASecondpoint: TDoublePoint;
  ARect: TDoubleRect): Boolean;
begin
 Result := false;
  with ARect do begin
    if AreSegmentsIntersect(AFirstpoint,ASecondpoint, DoublePoint(Left, Bottom), TopLeft) or
       AreSegmentsIntersect(AFirstpoint,ASecondpoint, TopLeft, DoublePoint(Right, Top)) or
       AreSegmentsIntersect(AFirstpoint,ASecondpoint, DoublePoint(Right, Top), BottomRight) or
       AreSegmentsIntersect(AFirstpoint,ASecondpoint, BottomRight, DoublePoint(Left, Bottom)) or
       ((AFirstpoint.Y <= max(Top,Bottom)) and (AFirstpoint.Y >= min(Top,Bottom)) and
       (AFirstpoint.X <= max(Left,Right)) and (AFirstpoint.X >= min(Left,Right)))
    then Result := True;
  end;
end;

procedure TTwoPointsFigure.Move(ADoublePoint: TDoublePoint);
begin
  Bounds.BottomRight += ADoublePoint;
  Bounds.TopLeft += ADoublePoint;
end;

  { TRectangle }

constructor TRectangle.Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
  AWidth: integer; ABrushStyle: TFPBrushStyle);
begin
  FLineColor  := APenColor;
  FLineWidth  := AWidth;
  FLineStyle  := APenStyle;
  FBrushColor := ABrushColor;
  FBrushStyle := ABrushStyle;
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  if Selected then begin
    DrawSelectFrame(Bounds, Canvas);
  end;
  Canvas.Pen.Color   := FLineColor;
  Canvas.Brush.Color := FBrushColor;
  Canvas.Pen.Width   := FLineWidth;
  Canvas.Brush.Style := FBrushStyle;
  Canvas.Pen.Style   := FLineStyle;
  Canvas.Rectangle(WorldToScreen(Bounds));
end;

function TRectangle.IsIntersect(ABounds: TDoubleRect): Boolean;
var
  Rect: HRGN;
begin
  with WorldToScreen(GetBounds) do begin
    Rect := CreateRectRgn(Left, Top, Right, Bottom);
  end;
  Result := RectInRegion(Rect, WorldToScreen(ABounds));
  DeleteObject(Rect);
end;

function TRectangle.IsPointInside(ABounds: TDoubleRect): Boolean;
var
  Point: TPoint;
  Rect: HRGN;
begin
  with WorldToScreen(GetBounds) do begin
    Rect := CreateRectRgn(Left, Top, Right, Bottom);
  end;
  Point := WorldToScreen(DoublePoint(ABounds.Left, ABounds.Top));
  Result := PtInRegion(Rect, Point.X, Point.Y);
  DeleteObject(Rect);
end;

  { TRoundRect }

constructor TRoundRect.Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
   AWidth: integer; ABrushStyle: TFPBrushStyle; ARadiusX, ARadiusY: integer);
begin
  FLineColor  := APenColor;
  FLineWidth  := AWidth;
  FLineStyle  := APenStyle;
  FBrushColor := ABrushColor;
  FBrushStyle := ABrushStyle;
  RadiusX     := ARadiusX;
  RadiusY     := ARadiusY;
end;

procedure TRoundRect.Draw(Canvas: TCanvas);
begin
  if Selected then begin
    DrawSelectFrame(Bounds, Canvas);
  end;
  Canvas.Pen.Color   := FLineColor;
  Canvas.Brush.Color := FBrushColor;
  Canvas.Pen.Width   := FLineWidth;
  Canvas.Brush.Style := FBrushStyle;
  Canvas.Pen.Style   := FLineStyle;
  Canvas.RoundRect(WorldToScreen(Bounds), RadiusX, RadiusY);
end;

function TRoundRect.IsIntersect(ABounds: TDoubleRect): Boolean;
var
  RoundRect: HRGN;
begin
  with WorldToScreen(GetBounds) do begin
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, RadiusX, RadiusY);
  end;
  Result := RectInRegion(RoundRect, WorldToScreen(ABounds));
  DeleteObject(RoundRect);
end;

function TRoundRect.IsPointInside(ABounds: TDoubleRect): Boolean;
var
  Point: TPoint;
  RoundRect: HRGN;
begin
  with WorldToScreen(GetBounds) do begin
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, RadiusX, RadiusY);
  end;
  Point := WorldToScreen(DoublePoint(ABounds.Left, ABounds.Top));
  Result := PtInRegion(RoundRect, Point.X, Point.Y);
  DeleteObject(RoundRect);
end;

  { TEllipse }

constructor TEllipse.Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
  AWidth: integer; ABrushStyle: TFPBrushStyle);
begin
  FLineColor  := APenColor;
  FLineWidth  := AWidth;
  FLineStyle  := APenStyle;
  FBrushColor := ABrushColor;
  FBrushStyle := ABrushStyle;
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  if Selected then begin
    DrawSelectFrame(Bounds, Canvas);
  end;
  Canvas.Pen.Color   := FLineColor;
  Canvas.Brush.Color := FBrushColor;
  Canvas.Pen.Width   := FLineWidth;
  Canvas.Brush.Style := FBrushStyle;
  Canvas.Pen.Style   := FLineStyle;
  Canvas.Ellipse(WorldToScreen(Bounds));
end;

function TEllipse.IsIntersect(ABounds: TDoubleRect): Boolean;
var
  Rect: HRGN;
begin
  with WorldToScreen(GetBounds) do begin
    Rect := CreateEllipticRgn(Left, Top, Right, Bottom);
  end;
  Result := RectInRegion(Rect, WorldToScreen(ABounds));
  DeleteObject(Rect);
end;

function TEllipse.IsPointInside(ABounds: TDoubleRect): Boolean;
var
  Point: TPoint;
  Ellipse: HRGN;
begin
  with WorldToScreen(GetBounds) do begin
    Ellipse := CreateEllipticRgn(Left, Top, Right, Bottom);
  end;
  Point := WorldToScreen(DoublePoint(ABounds.Left, ABounds.Top));
  Result := PtInRegion(Ellipse, Point.X, Point.Y);
  DeleteObject(Ellipse);
end;

  { TLine }

constructor TLine.Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
begin
  FLineColor := APenColor;
  FLineWidth := AWidth;
  FLineStyle := APenStyle;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  if Selected then begin
    DrawSelectFrame(Bounds, Canvas);
  end;
  Canvas.Pen.Color   := FLineColor;
  Canvas.Pen.Width   := FLineWidth;
  Canvas.Pen.Style   := FLineStyle;
  Canvas.Line(WorldToScreen(Bounds));
end;

function TLine.IsPointInside(ABounds: TDoubleRect): Boolean;
var
  B: TDoubleRect;
  Delta: integer = 2;
begin
  Result := false;
  B.Left   := min(ABounds.Left, ABounds.Right) - Delta;
  B.Top    := min(ABounds.Top, ABounds.Bottom) - Delta;
  B.Right  := min(ABounds.Left, ABounds.Right) + Delta;
  B.Bottom := min(ABounds.Top, ABounds.Bottom) + Delta;
 if IsRectIntersectSegment(GetBounds.TopLeft, GetBounds.BottomRight, B) then
   Result := true;
end;

function TLine.IsIntersect(ABounds: TDoubleRect): Boolean;
begin
  Result := false;
 if IsRectIntersectSegment(Bounds.TopLeft, Bounds.BottomRight, ABounds) then
   Result := true;
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
  FLineColor     := APenColor;
  FLineWidth     := AWidth;
  FLineStyle     := APenStyle;
  FBrushStyle    := ABrushStyle;
  FBrushColor    := ABrushColor;
  NumberOfAngles := ANumberOfAngles;
end;

procedure TPolygon.Draw(Canvas: TCanvas);
var
  i: Integer;
  R: Double;
  MidlCoord: TDoublePoint;
begin
 if Selected then begin
    DrawSelectFrame(Bounds, Canvas);
  end;
  Canvas.Pen.Color   := FLineColor;
  Canvas.Brush.Color := FBrushColor;
  Canvas.Pen.Width   := FLineWidth;
  Canvas.Brush.Style := FBrushStyle;
  Canvas.Pen.Style   := FLineStyle;
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

function TPolygon.IsIntersect(ABounds: TDoubleRect): Boolean;
var
  Polygon: HRGN;
  Points: array of TPoint;
begin
  SetLength(Points, NumberOfAngles);
  Points := WorldPointsToScreen(Angles);
  Polygon := CreatePolygonRgn(Points[0], Length(Points), WINDING);
  Result := RectInRegion(Polygon, WorldToScreen(ABounds));
  DeleteObject(Polygon);
end;

function TPolygon.IsPointInside(ABounds: TDoubleRect): Boolean;
var
  Polygon: HRGN;
  Points: array of TPoint;
  Point: TPoint;
begin
  SetLength(Points, NumberOfAngles);
  Points := WorldPointsToScreen(Angles);
  Polygon := CreatePolygonRgn(Points[0], Length(Points), WINDING);
  Point := WorldToScreen(DoublePoint(ABounds.Left, ABounds.Top));
  Result := PtInRegion(Polygon, Point.x, Point.y);
  DeleteObject(Polygon);
end;

end.

