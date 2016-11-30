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
    procedure Draw(Canvas: TCanvas); virtual; abstract;
  end;

  TPolyLine = class(TFigure)
    Points: array of TDoublePoint;
    constructor Create(APenColor: TColor; APenStyle: TFPPenStyle; AWidth: integer);
    procedure AddPoint(X, Y: Integer);
    procedure Draw(Canvas: TCanvas); override;
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

 { TRoundRect = class(TTwoPointsFigure)
    procedure DrawRoundRect(Canvas: TCanvas; ARadiusX, ARadiusY: integer);
  end;  }


implementation


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
  Canvas.Polyline(WorldPointsToScreen(Points));
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
  Canvas.Rectangle(WorldToScreen(Bounds));
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
  Canvas.RoundRect(WorldToScreen(Bounds), RadiusX, RadiusY);
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
  Canvas.Line(WorldToScreen(Bounds));
end;

  { TFrame }

procedure TFrame.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
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

  { TRoundRect }

{procedure TRoundRect.DrawRoundRect(Canvas: TCanvas; ARadiusX, ARadiusY: Integer);
begin
  Canvas.Pen.Color   := PenColor;
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Width   := Width;
  Canvas.Brush.Style := BrushStyle;
  Canvas.Pen.Style   := PenStyle;
  RadiusX:=5;
  RadiusY:=5;
  Canvas.RoundRect(WorldToScreen(Bounds), RadiusX, RadiusY);
end;  }

end.

