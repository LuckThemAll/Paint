unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, windows, Graphics, UScale, math, FPCanvas, TypInfo, strutils;

Type

  StrArr = array of String;

  TFigure = class
  public
    FLineColor: TColor;
    FBrushColor: TColor;
    Selected: Boolean;
    Bounds : TDoubleRect;
    function GetBounds: TDoubleRect; virtual;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure Move(ADoublePoint: TDoublePoint); virtual; abstract;
    procedure DrawSelectFrame(ABounds: TDoubleRect; ACanvas: TCanvas); virtual;
    procedure Load(AParameters: StrArr); virtual; abstract;
    procedure SetMaxBounds(ABounds: TDoubleRect);
    procedure UpdateBounds(AX, AY: Double);virtual;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; virtual; abstract;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   virtual; abstract;
    function AreSegmentsIntersect(A1, A2, B1, B2: TDoublePoint): Boolean;
    function Save: StrArr; virtual; abstract;
    function Copy: TFigure; virtual;
  end;

  ArrayOfTFigure = array of TFigure;

  TLinesFigure = class(TFigure)
  public
    FLineStyle: TFPPenStyle;
    FLineWidth: Integer;
    procedure Load(AParameters: StrArr); override;
    function Save: StrArr; override;
    function Copy: TFigure; override;
  end;

  TPolyLine = class(TLinesFigure)
  public
    Points: array of TDoublePoint;
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
    procedure AddPoint(X, Y: Integer);
    procedure Draw(Canvas: TCanvas); override;
    procedure Move(ADoublePoint: TDoublePoint); override;
    procedure Load(AParameters: StrArr); override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
    function Save: StrArr; override;
    function Copy: TFigure; override;
  end;

  TFilledFigures = class(TLinesFigure)
  public
    FBrushStyle: TFPBrushStyle;
    procedure Load(AParameters: StrArr); override;
    function Save: StrArr; override;
    function Copy: TFigure; override;
  end;

  { TTwoPointsFigure }

  TTwoPointsFigure = class(TFilledFigures)
  public
    procedure AddFirstPoint(X, Y: Integer);
    procedure AddSecondPoint(X, Y: Integer);
    procedure Move(ADoublePoint: TDoublePoint); override;
    procedure Load(AParameters: StrArr); override;
    function IsRectIntersectSegment(AFirstpoint, ASecondpoint: TDoublePoint;
      ARect: TDoubleRect): Boolean;
    function Save: StrArr; override;
    function Copy: TFigure; override;
  end;

  TRectangle = class(TTwoPointsFigure)
  public
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
  end;

  TRoundRect = class(TTwoPointsFigure)
  public
    RadiusX, RadiusY: integer;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle; ARadiusX, ARadiusY: integer);
    procedure Draw(Canvas: TCanvas); override;
    procedure Load(AParameters: StrArr); override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    function Save: StrArr; override;
    function Copy: TFigure; override;
  end;

  TEllipse = class(TTwoPointsFigure)
  public
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
  end;

  TLine = class(TTwoPointsFigure)
  public
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
    procedure Draw(Canvas: TCanvas); override;
    procedure Load(AParameters: StrArr); override;
    procedure DrawSelectFrame(ABounds: TDoubleRect; ACanvas: TCanvas); override;
    function GetBounds: TDoubleRect; override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    function Save: StrArr; override;
    function Copy: TFigure; override;
  end;

  TFrame = class(TTwoPointsFigure)
  public
    procedure Draw(Canvas: TCanvas); override;
  end;

  TPolygon = class(TTwoPointsFigure)
  public
    NumberOfAngles: integer;
    Angles: array of TDoublePoint;
    constructor Create(APenColor, ABrushColor: TColor; APenStyle: TFPPenStyle;
      AWidth: integer; ABrushStyle: TFPBrushStyle; ANumberOfAngles: Integer);
    procedure Draw(Canvas: TCanvas); override;
    procedure Load(AParameters: StrArr); override;
    function IsIntersect(ABounds: TDoubleRect): Boolean;   override;
    function IsPointInside(ADoublePoint: TDoublePoint): Boolean; override;
    function Save: StrArr; override;
    function Copy: TFigure; override;
  end;


  function SaveToStrArr: StrArr;

procedure SaveActualFigure(Figure: TFigure);

var
  Figures: ArrayOfTFigure;

implementation

procedure SaveActualFigure(Figure: TFigure);
begin
  SetLength(Figures, Length(Figures) + 1);
  Figures[High(Figures)] := Figure;
end;

function StrToStrPoint(AString: string; k: integer): String;
begin
  Result := '';
  while (AString[k] <> ' ') do begin
    Result += AString[k];
    Inc(k);
  end;
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

procedure TFigure.SetMaxBounds(ABounds: TDoubleRect);
begin
  Bounds.Top    := MaxInt;
  Bounds.Left   := MaxInt;
  Bounds.Bottom := -MaxInt;
  Bounds.Right  := -MaxInt;
end;

procedure TFigure.UpdateBounds(AX, AY: Double);
begin
  if Bounds.Left > AX then
    Bounds.Left := AX;
  if Bounds.Top > AY then
    Bounds.Top := AY;
  if Bounds.Right < AX then
    Bounds.Right := AX;
  if Bounds.Bottom < AY then
    Bounds.Bottom := AY;
end;

function SaveToStrArr: StrArr;
var
  i, j: Integer;
begin
  SetLength(Result, 1);
  Result[0] := IntToStr(Length(Figures));
  for i := Low(Figures) to High(Figures) do begin
    SetLength(Result, Length(Result) + Length(Figures[i].Save));
    for j := low((Figures[i]).Save) to high((Figures[i]).Save) do
      Result[High(Result) - high((Figures[i]).Save)+ j] := Figures[i].Save[j];
  end;
end;

function TFigure.Copy: TFigure;
begin
  case Self.ClassName of
   'TPolyLine' : Result := TPolyLine.Create(clBlack, psSolid, 1);
   'TLine'     : Result := TLine.Create(clBlack, psSolid, 1);
   'TRectangle': Result := TRectangle.Create(clBlack, clBlack, psSolid, 1, bsSolid);
   'TEllipse'  : Result := TEllipse.Create(clBlack, clBlack, psSolid, 1, bsSolid);
   'TRoundRect': Result := TRoundRect.Create(clBlack, clBlack, psSolid, 1, bsSolid, 0, 0);
   'TPolygon'  : Result := TPolygon.Create(clBlack, clBlack, psSolid, 1, bsSolid, 3);
  end;
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

function TPolyLine.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var
  i: integer;
  A: array of TPoint;
  B: TPoint;
  DistanceMouseToPoint: integer;
begin
  Result := False;
  A := WorldPointsToScreen(Points);
  B := WorldToScreen(ADoublePoint);
  for i := Low(A) to High(A) do begin
    DistanceMouseToPoint := round(sqrt((A[i].X - B.X)**2 + (A[i].Y - B.Y)**2));
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

function TPolyLine.Save: StrArr;
var
  i: integer;
begin
  SetLength(Result, 6);
  Result[0] := ClassName;
  Result[1] := IntToStr(Length(Result) - 2);
  Result[2] := FloatToStr(Points[0].X) + ' ' + FloatToStr(Points[0].Y);
  for i := (Low(Points) + 1) to High(Points) do
    Result[2] := Result[2] + ' ' + FloatToStr(Points[i].X) + ' ' + FloatToStr(Points[i].Y);
  Result[3] := IntToStr(FLineWidth);
  Result[4] := GetEnumName(TypeInfo(TFPPenStyle),Ord(FLineStyle));
  Result[5] := ColorToString(FLineColor);
end;

procedure TPolyLine.Load(AParameters: StrArr);
var
  i, NewPoint: integer;
  p: String;
begin
  i := 1;
  NewPoint := 0;
  SetMaxBounds(Bounds);
  AParameters[0] += ' ';
  while i <  Length(AParameters[0]) do begin
    SetLength(Points, Length(Points) + 1);
    p := StrToStrPoint(AParameters[0], i);
    i += Length(p) + 1;
    Points[NewPoint].X := StrToFloat(p);
    p := StrToStrPoint(AParameters[0], i);
    i += Length(p) + 1;
    Points[NewPoint].Y := StrToFloat(p);
    with Points[NewPoint] do
      UpdateBounds(X, Y);
    Inc(NewPoint);
  end;
  FLineWidth := StrToInt(AParameters[1]);
  FLineStyle := TFPPenStyle(GetEnumValue(TypeInfo(TFPPenStyle), AParameters[2]));
  FLineColor := StringToColor(AParameters[3]);
end;

function TPolyLine.Copy: TFigure;
var
  i: integer;
begin
  Result := Inherited;
  SetLength((Result as TPolyLine).Points, Length(Self.Points));
  for i := 0 to High(Self.Points) do
    (Result as TPolyLine).Points[i] := Self.Points[i];
  Result.Bounds.Top := Self.Bounds.Top;
  Result.Bounds.Left := Self.Bounds.Left;
  Result.Bounds.Bottom := Self.Bounds.Bottom;
  Result.Bounds.Right := Self.Bounds.Right;
  (Result as TLinesFigure).FLineWidth := Self.FLineWidth;
  (Result as TLinesFigure).FLineStyle := Self.FLineStyle;
  (Result as TLinesFigure).FLineColor := Self.FLineColor;
end;

  { TLinesFigure }

function TLinesFigure.Save: StrArr;
begin
  SetLength(Result, 6);
  Result[0] := ClassName;
  Result[1] := IntToStr(Length(Result) - 1);
  with Bounds do
  Result[2] := Format('%s %s %s %s', [FloatToStr(Min(Bounds.Top, Bounds.Bottom)),
                                      FloatToStr(Min(Bounds.Left, Bounds.Right)),
                                      FloatToStr(Max(Bounds.Top, Bounds.Bottom)),
                                      FloatToStr(Max(Bounds.Left, Bounds.Right))]);
  Result[3] := IntToStr(FLineWidth);
  Result[4] := GetEnumName(TypeInfo(TFPPenStyle),ord(FLineStyle));
  Result[5] := ColorToString(FLineColor);
end;

procedure TLinesFigure.Load(AParameters: StrArr);
var
  i: integer;
  p: String;
begin
  i := 1;
  while i <  Length(AParameters[0]) do begin
    p := StrToStrPoint(AParameters[0], i);
    i += Length(p) + 1;
    Bounds.Top := StrToFloat(p);
    p := StrToStrPoint(AParameters[0], i);
    i += Length(p) + 1;
    Bounds.Left := StrToFloat(p);
    p := StrToStrPoint(AParameters[0], i);
    i += Length(p) + 1;
    Bounds.Bottom := StrToFloat(p);
    p := StrToStrPoint(AParameters[0], i);
    i += Length(p) + 1;
    Bounds.Right := StrToFloat(p);
    with Bounds do begin
      UpdateBounds(Left, Top);
      UpdateBounds(Right, Bottom);
    end;
  end;
  FLineWidth := StrToInt(AParameters[1]);
  FLineStyle := TFPPenStyle(GetEnumValue(TypeInfo(TFPPenStyle), AParameters[2]));
  FLineColor := StringToColor(AParameters[3]);
end;

function TLinesFigure.Copy: TFigure;
var
  i: integer;
begin
  Inherited;
  Result := Inherited;
  Result.Bounds.Top := Self.Bounds.Top;
  Result.Bounds.Left := Self.Bounds.Left;
  Result.Bounds.Bottom := Self.Bounds.Bottom;
  Result.Bounds.Right := Self.Bounds.Right;
  (Result as TLinesFigure).FLineWidth := Self.FLineWidth;
  (Result as TLinesFigure).FLineStyle := Self.FLineStyle;
  (Result as TLinesFigure).FLineColor := Self.FLineColor;
end;

  { TFilledFigure }

function TFilledFigures.Save: StrArr;
begin
  Inherited;
  Result := Inherited;
  SetLength(Result, Length(Result) + 2);
  Result[1]                := IntToStr(Length(Result) - 2);
  Result[High(Result) - 1] := GetEnumName(TypeInfo(TBrushStyle),Ord(FBrushStyle));
  Result[High(Result)]     := ColorToString(FBrushColor);
end;

procedure TFilledFigures.Load(AParameters: StrArr);
begin
  Inherited;
  FBrushStyle := TBrushStyle(GetEnumValue(TypeInfo(TBrushStyle), AParameters[4]));
  FBrushColor := StringToColor(AParameters[5]);
end;

function TFilledFigures.Copy: TFigure;
var
  i: integer;
begin
  Inherited;
  Result := Inherited;
  (Result as TFilledFigures).FBrushStyle := Self.FBrushStyle;
  (Result as TFilledFigures).FBrushColor := Self.FBrushColor;
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

function TTwoPointsFigure.Save: StrArr;
begin
  Inherited;
  Result := Inherited;
end;

procedure TTwoPointsFigure.Load(AParameters: StrArr);
begin
  Inherited;
end;

function TTwoPointsFigure.Copy: TFigure;
begin
  Inherited;
  Result := Inherited;
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

function TRectangle.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var
  Point: TPoint;
  Rect: HRGN;
begin
  with WorldToScreen(GetBounds) do begin
    Rect := CreateRectRgn(Left, Top, Right, Bottom);
  end;
  Point := WorldToScreen(ADoublePoint);
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

function TRoundRect.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var
  Point: TPoint;
  RoundRect: HRGN;
begin
  with WorldToScreen(GetBounds) do begin
    RoundRect := CreateRoundRectRgn(Left, Top, Right, Bottom, RadiusX, RadiusY);
  end;
  Point := WorldToScreen(ADoublePoint);
  Result := PtInRegion(RoundRect, Point.X, Point.Y);
  DeleteObject(RoundRect);
end;

function TRoundRect.Save: StrArr;
begin
  Result := Inherited;
  SetLength(Result, Length(Result) + 2);
  Result[1] := IntToStr(Length(Result) - 2);
  Result[High(Result) - 1] := IntToStr(RadiusX);
  Result[High(Result)] := IntToStr(RadiusY);
end;

procedure TRoundRect.Load(AParameters: StrArr);
begin
  Inherited;
  RadiusX := StrToInt(AParameters[High(AParameters) - 1]);
  RadiusY := StrToInt(AParameters[High(AParameters)]);;
end;

function TRoundRect.Copy: TFigure;
begin
  Inherited;
  Result := Inherited;
  (Result as TRoundRect).RadiusX := Self.RadiusX;
  (Result as TRoundRect).RadiusY := Self.RadiusY;
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

function TEllipse.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var
  Point: TPoint;
  Ellipse: HRGN;
begin
  with WorldToScreen(GetBounds) do begin
    Ellipse := CreateEllipticRgn(Left, Top, Right, Bottom);
  end;
  Point := WorldToScreen(ADoublePoint);
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

function TLine.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var
  B: TDoublePoint;
  Delta: integer = 2;
begin
  Result := false;
  B.X   := ADoublePoint.X - Delta;
  B.Y   := ADoublePoint.Y - Delta;
  with GetBounds, B do
 if IsRectIntersectSegment(TopLeft, BottomRight, DoubleRect(X, Y, X, Y)) then
   Result := true;
end;

function TLine.IsIntersect(ABounds: TDoubleRect): Boolean;
begin
  Result := false;
 if IsRectIntersectSegment(Bounds.TopLeft, Bounds.BottomRight, ABounds) then
   Result := true;
end;

procedure TLine.DrawSelectFrame(ABounds: TDoubleRect; ACanvas: TCanvas);
var
  Delta: integer = 7;
begin
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psDash;
  if abs(ABounds.Top - ABounds.Bottom) <= 4 then begin
    ABounds.Top -= Delta;
    ABounds.Bottom += Delta;
  end;
  if abs(ABounds.Left - ABounds.Right) <= 4 then begin
    ABounds.Left -= Delta;
    ABounds.Right += Delta;
  end;
  ACanvas.Frame(WorldToScreen(ABounds));
end;

function TLine.GetBounds: TDoubleRect;
begin
 with Result do begin
   Top    := Bounds.Top;
   Left   := Bounds.Left;
   Bottom := Bounds.Bottom;
   Right  := Bounds.Right;
 end;
end;

function TLine.Save: StrArr;
begin
  SetLength(Result, 6);
  Result[0] := ClassName;
  Result[1] := IntToStr(Length(Result) - 2);
  Result[2] := FloatToStr(Min(Bounds.Top, Bounds.Bottom)) + ' ' +
               FloatToStr(Min(Bounds.Left, Bounds.Right)) + ' ' +
               FloatToStr(Max(Bounds.Top, Bounds.Bottom)) + ' ' +
               FloatToStr(Max(Bounds.Left, Bounds.Right));
  Result[3] := IntToStr(FLineWidth);
  Result[4] := GetEnumName(TypeInfo(TFPPenStyle),Ord(FLineStyle));
  Result[5] := ColorToString(FLineColor);
end;

procedure TLine.Load(AParameters: StrArr);
var
  i: integer;
  p: String;
begin
  i := 1;
  p := StrToStrPoint(AParameters[0], i);
  i += Length(p) + 1;
  Bounds.Top := StrToFloat(p);
  p := StrToStrPoint(AParameters[0], i);
  i += Length(p) + 1;
  Bounds.Left := StrToFloat(p);
  p := StrToStrPoint(AParameters[0], i);
  i += Length(p) + 1;
  Bounds.Bottom := StrToFloat(p);
  p := StrToStrPoint(AParameters[0], i);
  i += Length(p) + 1;
  Bounds.Right := StrToFloat(p);
  with Bounds do begin
    UpdateBounds(Left, Top);
    UpdateBounds(Right, Bottom);
  end;
  FLineWidth := StrToInt(AParameters[1]);
  FLineStyle := TFPPenStyle(GetEnumValue(TypeInfo(TFPPenStyle), AParameters[2]));
  FLineColor := StringToColor(AParameters[3]);
end;

function TLine.Copy: TFigure;
var
  i: Integer;
begin
  Result := TLine.Create(clBlack, psSolid, 1);
  Result.Bounds.Top := Self.Bounds.Top;
  Result.Bounds.Left := Self.Bounds.Left;
  Result.Bounds.Bottom := Self.Bounds.Bottom;
  Result.Bounds.Right := Self.Bounds.Right;
  (Result as TLine).FLineWidth := Self.FLineWidth;
  (Result as TLine).FLineStyle := Self.FLineStyle;
  (Result as TLine).FLineColor := Self.FLineColor;
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

function TPolygon.IsPointInside(ADoublePoint: TDoublePoint): Boolean;
var
  Polygon: HRGN;
  Points: array of TPoint;
  ABounds: TDoubleRect;
begin
  SetLength(Points, NumberOfAngles);
  Points := WorldPointsToScreen(Angles);
  ABounds.Top    := ADoublePoint.Y;
  ABounds.Left   := ADoublePoint.X;
  ABounds.Bottom := ADoublePoint.Y + 3;
  ABounds.Right  := ADoublePoint.X + 3;
  Polygon := CreatePolygonRgn(Points[0], Length(Points), WINDING);
  Result := RectInRegion(Polygon, WorldToScreen(ABounds));
  DeleteObject(Polygon);
end;

function TPolygon.Save: StrArr;
begin
Inherited;
  Result := Inherited;
  Result[1] := IntToStr(Length(Result) - 2);
  Result[High(Result)] := IntToStr(NumberOfAngles);
end;

procedure TPolygon.Load(AParameters: StrArr);
begin
  Inherited;
  NumberOfAngles := StrToInt(AParameters[High(AParameters)]);
end;

function TPolygon.Copy: TFigure;
begin
  Inherited;
  Result := Inherited;
  (Result as TPolygon).NumberOfAngles := Self.NumberOfAngles;
end;

end.

