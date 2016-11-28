unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics, UScale, math, ExtCtrls, Spin, StdCtrls,
   FPCanvas, Controls, UParameters;
type

  TTool = class
    FIcon: String;
    Figure: TFigure;
    FPanel: TPanel;
    ParametersAvailable: Boolean;
    function GetFigure: TFigure;
    procedure AddParameter(AParameter: TParameter);
    procedure Init(APanel: TPanel);
    procedure InitParameters; virtual; abstract;
    procedure ShowParameters;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  virtual; abstract;
    procedure MouseMove(X, Y: integer); virtual; abstract;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer);        virtual; abstract;
  end;

  THandTool = class(TTool)
    FCurrentPoint: TDoublePoint;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer);        override;
    procedure InitParameters;           override;
  end;

  TZoomInTool = class(TTool)
    FCurrentPoint: TDoublePoint;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer);        override;
    procedure InitParameters;           override;
  end;

  TZoomOutTool = class(TTool)
    FCurrentPoint: TDoublePoint;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer);        override;
    procedure InitParameters;           override;
  end;

  TZoomToTool = class(TTool)
    FFirstPoint, FSecondPoint: TDoublePoint;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer);        override;
    procedure InitParameters;           override;
  end;

  TPolyLineTool = class(TTool)
    FLineWidth: integer;
    FLineStyle: TFPPenStyle;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer);        override;
    procedure InitParameters;           override;
    procedure ChangeLineWidth(Sender: TObject);
    procedure ChangeLineStyle(Sender: TObject);
  end;

  TRectangleTool = class(TTool)
    FLineWidth: integer;
    FLineStyle: TFPPenStyle;
    FBrushStyle: TFPBrushStyle;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer);        override;
    procedure InitParameters;           override;
    procedure ChangeLineWidth(Sender: TObject);
    procedure ChangeLineStyle(Sender: TObject);
    procedure ChangeBrushStyle(Sender: TObject);
  end;

  TPolygonTool = class(TTool)
    FLineWidth, FNumberOfAngles: Integer;
    FLineStyle: TFPPenStyle;
    FBrushStyle: TFPBrushStyle;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer);        override;
    procedure InitParameters;           override;
    procedure ChangeLineWidth(Sender: TObject);
    procedure ChangeLineStyle(Sender: TObject);
    procedure ChangeBrushStyle(Sender: TObject);
    procedure ChangeNumberOfAngles(sender: TObject);
  end;

  TEllipseTool = class(TTool)
    FLineWidth: integer;
    FLineStyle: TFPPenStyle;
    FBrushStyle: TFPBrushStyle;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer);        override;
    procedure InitParameters;           override;
    procedure ChangeLineWidth(Sender: TObject);
    procedure ChangeLineStyle(Sender: TObject);
    procedure ChangeBrushStyle(Sender: TObject);
  end;

  TLineTool = class(TTool)
    FLineWidth: integer;
    FLineStyle: TFPPenStyle;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer);        override;
    procedure InitParameters;           override;
    procedure ChangeLineWidth(Sender: TObject);
    procedure ChangeLineStyle(Sender: TObject);
  end;

  {TRoundRectTool = class(TTool)
    constructor Create;
    procedure MouseDown(X, Y: integer); override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer);        override;
    procedure InitParameters;           override;
  end;                      }


var
  ToolRegistry: array of TTool;

implementation
var
  Parameters: array of TParameter;

  { TTool }

procedure RegisterTool(Tool: TTool);
begin
  SetLength(ToolRegistry, Length(ToolRegistry) + 1);
  ToolRegistry[High(ToolRegistry)] := Tool;
end;

procedure TTool.AddParameter(AParameter: TParameter);
begin
  SetLength(Parameters, Length(Parameters) + 1);
  Parameters[High(Parameters)] := AParameter;
end;

procedure TTool.Init(APanel: TPanel);
var
  i: Integer;
begin
  Parameters := nil;
  FPanel := APanel;
  InitParameters;
  ShowParameters;
  for i := Low(Parameters) to High(Parameters) do begin
    FPanel.Height :=(
      FPanel.Height +
      Parameters[1].FLabel.Height +
      Parameters[i].FComponent.Height);
  end;
end;

procedure TTool.ShowParameters;
var
  i: integer;
begin
  for i := 0 to High(Parameters) do begin
    with Parameters[i] do begin
      FLabel.Top        := i * 50;
      FLabel.Left       := 2;
      FLabel.Parent     := FPanel;
      FComponent.Top    := i * 50 + FLabel.ClientHeight + 5;
      FComponent.Left   := 2;
      FComponent.Parent := FPanel;
    end;
  end;
end;

function TTool.GetFigure: TFigure;
begin
  Result := Figure;
end;


  { TZoomInTool }

constructor TZoomInTool.Create;
begin
  Inherited;
  FIcon := 'imgs/ZoomIn.bmp';
end;

procedure TZoomInTool.MouseDown(X, Y: integer; APenColor, ABrushColor: TColor);
begin
  FCurrentPoint := ScreenToWorld(X, Y);
end;

procedure TZoomInTool.MouseMove(X, Y: integer);
begin
  FCurrentPoint := ScreenToWorld(X, Y);
end;

procedure TZoomInTool.MouseUp(X, Y: integer; AWidth, AHeight: Integer);
begin
  if scale * 2 <= 100 then begin
    Scale := Scale * 2;
    ChangeScreenCoords(
      FCurrentPoint.X - ScreenToWorldX(X),
      FCurrentPoint.Y - ScreenToWorldY(Y));
  end;
end;

procedure TZoomInTool.InitParameters;
begin
  ParametersAvailable := False;
end;

  { TZoomOutTool }

constructor TZoomOutTool.Create;
begin
  Inherited;
  FIcon := 'imgs/ZoomOut.bmp';
end;

procedure TZoomOutTool.MouseDown(X, Y: integer; APenColor, ABrushColor: TColor);
begin
  FCurrentPoint := ScreenToWorld(X, Y);
end;

procedure TZoomOutTool.MouseMove(X, Y: integer);
begin
  FCurrentPoint := ScreenToWorld(X, Y);
end;

procedure TZoomOutTool.MouseUp(X, Y: integer; AWidth, AHeight: Integer);
begin
  if scale / 2 >= 0.01 then begin
    Scale := Scale / 2;
    ChangeScreenCoords(
      FCurrentPoint.X - ScreenToWorldX(X),
      FCurrentPoint.Y - ScreenToWorldY(Y));
  end;
end;

procedure TZoomOutTool.InitParameters;
begin
  ParametersAvailable := False;
end;

  { TZoomToTool }

constructor TZoomToTool.Create;
begin
  Inherited;
  FIcon := 'imgs/ZoomTo.bmp';
end;

procedure TZoomToTool.MouseDown(X, Y: integer; APenColor, ABrushColor: TColor);
begin
  Figure := TFrame.Create;
  (Figure as TFrame).AddFirstPoint(X, Y);
  FFirstPoint := ScreenToWorld(X, Y);
end;

procedure TZoomToTool.MouseMove(X, Y: integer);
var
  i : integer;
begin
  (Figure as TFrame).AddSecondPoint(X, Y);
  FSecondPoint := ScreenToWorld(X, Y);
end;

procedure TZoomToTool.MouseUp(X, Y: integer; AWidth, AHeight: Integer);
var
  ZoomWidth, ZoomHeigth, MidX, MidY: Double;
begin
  (Figure as TFrame).AddSecondPoint(X, Y);
  ZoomWidth := abs(FSecondPoint.X - FFirstPoint.X);
  ZoomHeigth := abs(FSecondPoint.Y - FFirstPoint.Y);
  MidX := min(FFirstPoint.X, FSecondPoint.X);
  MidY := min(FFirstPoint.Y, FSecondPoint.Y);
  Scale := Min(AWidth / ZoomWidth, AHeight / ZoomHeigth);
  UScale.SetScreenCoords(MidX, MidY);
  Figure := nil;
end;

procedure TZoomToTool.InitParameters;
begin
  ParametersAvailable := False;
end;

  { THandTool }

constructor THandTool.Create;
begin
  Inherited;
  FIcon := 'imgs/Hand.bmp';
end;

procedure THandTool.MouseDown(X, Y: integer; APenColor, ABrushColor: TColor);
begin
  FCurrentPoint := ScreenToWorld(X, Y);
end;

procedure THandTool.MouseMove(X, Y: integer);
begin
  ChangeScreenCoords(
    FCurrentPoint.X - ScreenToWorldX(X),
    FCurrentPoint.Y - ScreenToWorldY(Y));
end;

procedure THandTool.MouseUp(X, Y: integer; AWidth, AHeight: Integer);
begin
  ChangeScreenCoords(
    FCurrentPoint.X - ScreenToWorldX(X),
    FCurrentPoint.Y - ScreenToWorldY(Y));
end;

procedure THandTool.InitParameters;
begin
  ParametersAvailable := False;
end;

  { TPolylineTool }

constructor TPolyLineTool.Create;
begin
  Inherited;
  FIcon := 'imgs/PolyLine.bmp';
end;

procedure TPolyLineTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor);
begin
  Figure := TPolyline.Create(APenColor, FLineStyle, FLineWidth);
  (Figure as TPolyline).AddPoint(X, Y);
end;

procedure TPolyLineTool.MouseMove(X, Y: Integer);
begin
  (Figure as TPolyline).AddPoint(X, Y);
end;

procedure TPolyLineTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer);
begin
  (Figure as TPolyline).AddPoint(X, Y);
end;

procedure TPolylineTool.InitParameters;
begin
  ParametersAvailable := True;
  AddParameter(TBorderWidthParameter.Create(@ChangeLineWidth));
  AddParameter(TBorderStyleParameter.Create(@ChangeLineStyle));
end;

procedure TPolyLineTool.ChangeLineWidth(Sender: TObject);
begin
  FLineWidth := (Sender as TSpinEdit).Value;
end;


procedure TPolyLineTool.ChangeLineStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FLineStyle := TFPPenStyle(ItemIndex);
  end;
end;

  { TRectangleTool }

constructor TRectangleTool.Create;
begin
  Inherited;
  FIcon := 'imgs/Rectangle.bmp';
end;

procedure TRectangleTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor);
begin
  Figure := TRectangle.Create(APenColor, ABrushColor, FLineStyle, FLineWidth, FBrushStyle);
  (Figure as TRectangle).AddFirstPoint(X, Y);
end;

procedure TRectangleTool.MouseMove(X, Y: Integer);
begin
  (Figure as TRectangle).AddSecondPoint(X, Y);
end;

procedure TRectangleTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer);
begin
  (Figure as TRectangle).AddSecondPoint(X, Y);
end;

procedure TRectangleTool.InitParameters;
begin
  ParametersAvailable := True;
  AddParameter(TBorderWidthParameter.Create(@ChangeLineWidth));
  AddParameter(TBorderStyleParameter.Create(@ChangeLineStyle));
  AddParameter(TBrushStyleParameter.Create(@ChangeBrushStyle));
end;

procedure TRectangleTool.ChangeLineWidth(Sender: TObject);
begin
  FLineWidth := (Sender as TSpinEdit).Value;
end;


procedure TRectangleTool.ChangeLineStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FLineStyle := TFPPenStyle(ItemIndex);
  end;
end;

procedure TRectangleTool.ChangeBrushStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FBrushStyle := TFPBrushStyle(ItemIndex);
  end;
end;

  { TPolygonTool }

constructor TPolygonTool.Create;
begin
  Inherited;
  FIcon := 'imgs/Polygon.bmp';
end;

procedure TPolygonTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor);
begin
  Figure := TPolygon.Create(APenColor, ABrushColor, FLineStyle, FLineWidth,
    FBrushStyle, FNumberOfAngles);
  (Figure as TPolygon).AddFirstPoint(X, Y);
end;

procedure TPolygonTool.MouseMove(X, Y: Integer);
begin
  (Figure as TPolygon).AddSecondPoint(X, Y);
end;

procedure TPolygonTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer);
begin
  (Figure as TPolygon).AddSecondPoint(X, Y);
end;

procedure TPolygonTool.InitParameters;
begin
  ParametersAvailable := True;
  AddParameter(TBorderWidthParameter.Create(@ChangeLineWidth));
  AddParameter(TBorderStyleParameter.Create(@ChangeLineStyle));
  AddParameter(TBrushStyleParameter.Create(@ChangeBrushStyle));
  AddParameter(TNumberOfAnglesParameter.Create(@ChangeNumberOfAngles));
end;

procedure TPolygonTool.ChangeLineWidth(Sender: TObject);
begin
  FLineWidth := (Sender as TSpinEdit).Value;
end;


procedure TPolygonTool.ChangeLineStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FLineStyle := TFPPenStyle(ItemIndex);
  end;
end;

procedure TPolygonTool.ChangeBrushStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FBrushStyle := TFPBrushStyle(ItemIndex);
  end;
end;

procedure TPolygonTool.ChangeNumberOfAngles(Sender: TObject);
begin
  FNumberOfAngles := (Sender as TSpinEdit).Value;
end;

  { TEllipseTool }

constructor TEllipseTool.Create;
begin
  Inherited;
  FIcon := 'imgs/Ellipse.bmp';
end;

procedure TEllipseTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor);
begin
  Figure := TEllipse.Create(APenColor, ABrushColor, FLineStyle, FLineWidth, FBrushStyle);
  (Figure as TEllipse).AddFirstPoint(X, Y);
end;

procedure TEllipseTool.MouseMove(X, Y: Integer);
begin
  (Figure as TEllipse).AddSecondPoint(X, Y);
end;

procedure TEllipseTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer);
begin
  (Figure as TEllipse).AddSecondPoint(X, Y);
end;

procedure TEllipseTool.InitParameters;
begin
  ParametersAvailable := True;
  AddParameter(TBorderWidthParameter.Create(@ChangeLineWidth));
  AddParameter(TBorderStyleParameter.Create(@ChangeLineStyle));
  AddParameter(TBrushStyleParameter.Create(@ChangeBrushStyle));
end;

procedure TEllipseTool.ChangeLineWidth(Sender: TObject);
begin
  FLineWidth := (Sender as TSpinEdit).Value;
end;


procedure TEllipseTool.ChangeLineStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FLineStyle := TFPPenStyle(ItemIndex);
  end;
end;

procedure TEllipseTool.ChangeBrushStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FBrushStyle := TFPBrushStyle(ItemIndex);
  end;
end;

  { TLineTool }

constructor TLineTool.Create;
begin
  Inherited;
  FIcon := 'imgs/Line.bmp';
end;

procedure TLineTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor);
begin
  Figure := TLine.Create(APenColor, FLineStyle, FLineWidth);
  (Figure as TLine).AddFirstPoint(X, Y);
end;

procedure TLineTool.MouseMove(X, Y: Integer);
begin
  (Figure as TLine).AddSecondPoint(X, Y);
end;

procedure TLineTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer);
begin
  (Figure as TLine).AddSecondPoint(X, Y);
end;

procedure TLineTool.InitParameters;
begin
  ParametersAvailable := True;
  AddParameter(TBorderWidthParameter.Create(@ChangeLineWidth));
  AddParameter(TBorderStyleParameter.Create(@ChangeLineStyle));
end;

procedure TLineTool.ChangeLineWidth(Sender: TObject);
begin
  FLineWidth := (Sender as TSpinEdit).Value;
end;

procedure TLineTool.ChangeLineStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FLineStyle := TFPPenStyle(ItemIndex);
  end;
end;


{  { TRoundRectTool }

constructor TRoundRectTool.Create;
begin
  Inherited;
  FIcon := 'imgs/RoundRect.bmp';
end;

procedure TRoundRectTool.MouseDown(X, Y: Integer);
begin
  Figure := TRoundRect.Create;
  (Figure as TRoundRect).AddFirstPoint(X, Y);
end;

procedure TRoundRectTool.MouseMove(X, Y: Integer);
begin
  (Figure as TRoundRect).AddSecondPoint(X, Y);
end;

procedure TRoundRectTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer);
begin
  (Figure as TRoundRect).AddSecondPoint(X, Y);
end;

procedure TRoundRectTool.InitParameters;
begin
  ParametersAvailable := True;
end;}

initialization
  RegisterTool(THandTool.Create);
  RegisterTool(TPolyLineTool.Create);
  RegisterTool(TRectangleTool.Create);
  RegisterTool(TEllipseTool.Create);
  RegisterTool(TLineTool.Create);
  RegisterTool(TPolygonTool.Create);
  //RegisterTool(TRoundRectTool.Create);
  RegisterTool(TZoomInTool.Create);
  RegisterTool(TZoomOutTool.Create);
  RegisterTool(TZoomToTool.Create);
end.

