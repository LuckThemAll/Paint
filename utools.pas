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
      AWidth, AHeight: Integer;  Shift: TShiftState); virtual; abstract;
  end;

  THandTool = class(TTool)
    FCurrentPoint: TDoublePoint;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters;           override;
  end;

  TZoomTool = class(TTool)
    FFirstPoint, FSecondPoint: TDoublePoint;
    ZoomKoef: Double;
    ZoomParameter: String;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters;           override;
    procedure ChangeZoomParameter(Sender: TObject);
  end;

  TPolyLineTool = class(TTool)
    FLineWidth: integer;
    FLineStyle: TFPPenStyle;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
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
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
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
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
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
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
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
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters;           override;
    procedure ChangeLineWidth(Sender: TObject);
    procedure ChangeLineStyle(Sender: TObject);
  end;


  TRoundRectTool = class(TTool)
    FLineWidth: integer;
    FLineStyle: TFPPenStyle;
    FBrushStyle: TFPBrushStyle;
    RadiusX, RadiusY: integer;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters;           override;
    procedure ChangeLineWidth(Sender: TObject);
    procedure ChangeLineStyle(Sender: TObject);
    procedure ChangeBrushStyle(Sender: TObject);
    procedure ChangeRadiusX(Sender: TObject);
    procedure ChangeRadiusY(Sender: TObject);
  end;

  TSelectTool = class(TTool)
    FFirstPoint, FSecondPoint: TDoublePoint;
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters;           override;
  end;


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
      Parameters[i].FLabel.Height +
      Parameters[i].FComponent.Height);
  end;
end;

procedure TTool.ShowParameters;
var
  i: integer;
begin
  for i := 0 to High(Parameters) do begin
    with Parameters[i] do begin
      FLabel.Top        := i * 47;
      FLabel.Left       := 2;
      FLabel.Parent     := FPanel;
      FComponent.Top    := i * 47 + FLabel.ClientHeight;
      FComponent.Left   := 2;
      FComponent.Parent := FPanel;
    end;
  end;
end;

function TTool.GetFigure: TFigure;
begin
  Result := Figure;
end;

  { TZoomTool }

constructor TZoomTool.Create;
begin
  Inherited;
  FIcon := 'imgs/Zoom.bmp';
end;

procedure TZoomTool.MouseDown(X, Y: integer; APenColor, ABrushColor: TColor);
begin
  if ZoomParameter = '' then ZoomParameter := 'Увеличить';
  case ZoomParameter of
    'Увеличить': begin
       ZoomKoef := 1.2;
       FFirstPoint := ScreenToWorld(X, Y);
    end;
    'Уменьшить': begin
      ZoomKoef := 0.8;
      FFirstPoint := ScreenToWorld(X, Y);
    end;
    'Приблизить область': begin
      Figure := TFrame.Create;
      (Figure as TFrame).AddFirstPoint(X, Y);
      FFirstPoint := ScreenToWorld(X, Y);
    end;
  end;
end;

procedure TZoomTool.MouseMove(X, Y: integer);
var
  i : integer;
begin
  case ZoomParameter of
    'Приблизить область': begin
      (Figure as TFrame).AddSecondPoint(X, Y);
      FSecondPoint := ScreenToWorld(X, Y);
    end;
  end;
end;

procedure TZoomTool.MouseUp(X, Y: integer; AWidth, AHeight: Integer; Shift: TShiftState);
var
  ZoomWidth, ZoomHeigth, MidX, MidY: Double;
begin
  case ZoomParameter of
    'Увеличить': begin
      if Scale * ZoomKoef <= 100 then begin
        Scale := Scale * ZoomKoef;
        ChangeScreenCoords(
          FFirstPoint.X - ScreenToWorldX(X),
          FFirstPoint.Y - ScreenToWorldY(Y));
      end;
    end;
    'Уменьшить': begin
      if Scale * ZoomKoef >= 0.01 then begin
       Scale := Scale * ZoomKoef;
       ChangeScreenCoords(
         FFirstPoint.X - ScreenToWorldX(X),
         FFirstPoint.Y - ScreenToWorldY(Y));
      end;
    end;
    'Приблизить область': begin
      (Figure as TFrame).AddSecondPoint(X, Y);
      ZoomWidth := abs(FSecondPoint.X - FFirstPoint.X);
      ZoomHeigth := abs(FSecondPoint.Y - FFirstPoint.Y);
      MidX := min(FFirstPoint.X, FSecondPoint.X);
      MidY := min(FFirstPoint.Y, FSecondPoint.Y);
      Scale := Min(AWidth / ZoomWidth, AHeight / ZoomHeigth);
      UScale.SetScreenCoords(MidX, MidY);
      Figure := nil;
    end;
  end;
end;

procedure TZoomTool.InitParameters;
begin
  ParametersAvailable := true;
  AddParameter(TZoomParameter.Create(@ChangeZoomParameter));
end;

procedure TZoomTool.ChangeZoomParameter(Sender: TObject);
begin
  with Sender as TComboBox do begin
    ZoomParameter := Text;
  end;
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
var
  i, j: Integer;
  Selected: Boolean;
  NewWorldCoords: TDoublePoint;
begin
  Selected := False;
  NewWorldCoords := (ScreenToWorld(X, Y) - FCurrentPoint);
  for i := Low(Figures) to High(Figures) do
    if Figures[i].Selected then
      Selected:=True;
  if Selected then
  for i := Low(Figures) to High(Figures) do
    if Figures[i].Selected then
      Figures[i].Move(NewWorldCoords);
  if not Selected then
    ChangeScreenCoords(
      FCurrentPoint.X - ScreenToWorldX(X),
      FCurrentPoint.Y - ScreenToWorldY(Y))
  else
    FCurrentPoint := ScreenToWorld(X, Y);
end;

procedure THandTool.MouseUp(X, Y: integer; AWidth, AHeight: Integer; Shift: TShiftState);
begin
  {ChangeScreenCoords(
    FCurrentPoint.X - ScreenToWorldX(X),
    FCurrentPoint.Y - ScreenToWorldY(Y));}
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
  UnselectAll;
  Figure := TPolyline.Create(APenColor, FLineStyle, FLineWidth);
  with Figure.Bounds do begin
    Top := ScreenToWorld(X, Y).Y;
    Left := ScreenToWorld(X, Y).X;
    Bottom := ScreenToWorld(X, Y).Y;
    Right := ScreenToWorld(X, Y).X;
  end;
  (Figure as TPolyline).AddPoint(X, Y);
end;

procedure TPolyLineTool.MouseMove(X, Y: Integer);
begin
  (Figure as TPolyline).AddPoint(X, Y);
end;

procedure TPolyLineTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer; Shift: TShiftState);
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

procedure TRectangleTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer; Shift: TShiftState);
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
  FNumberOfAngles := 3;
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

procedure TPolygonTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer; Shift: TShiftState);
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

procedure TEllipseTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer; Shift: TShiftState);
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

procedure TLineTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer; Shift: TShiftState);
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

{ TRounRectTool }

constructor TRoundRectTool.Create;
begin
  Inherited;
  FIcon := 'imgs/RoundRect.bmp';
end;

procedure TRoundRectTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor);
begin
  Figure := TRoundRect.Create(APenColor, ABrushColor, FLineStyle, FLineWidth, FBrushStyle,
    RadiusX, RadiusY);
  (Figure as TRoundRect).AddFirstPoint(X, Y);
end;

procedure TRoundRectTool.MouseMove(X, Y: Integer);
begin
  (Figure as TRoundRect).AddSecondPoint(X, Y);
end;

procedure TRoundRectTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer; Shift: TShiftState);
  begin
  (Figure as TRoundRect).AddSecondPoint(X, Y);
end;

procedure TRoundRectTool.InitParameters;
begin
  ParametersAvailable := True;
  AddParameter(TBorderWidthParameter.Create(@ChangeLineWidth));
  AddParameter(TBorderStyleParameter.Create(@ChangeLineStyle));
  AddParameter(TBrushStyleParameter.Create(@ChangeBrushStyle));
  AddParameter(TRadiusXRoundRectParameter.Create(@ChangeRadiusX));
  AddParameter(TRadiusYRoundRectParameter.Create(@ChangeRadiusY));
end;

procedure TRoundRectTool.ChangeLineWidth(Sender: TObject);
begin
  FLineWidth := (Sender as TSpinEdit).Value;
end;


procedure TRoundRectTool.ChangeLineStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FLineStyle := TFPPenStyle(ItemIndex);
  end;
end;

procedure TRoundRectTool.ChangeBrushStyle(Sender: TObject);
begin
  with Sender as TComboBox do begin
    FBrushStyle := TFPBrushStyle(ItemIndex);
  end;
end;

procedure TRoundRectTool.ChangeRadiusX(Sender: TObject);
begin
  with Sender as TSpinEdit do begin
    RadiusX := (Sender as TSpinEdit).Value;
  end;
end;

procedure TRoundRectTool.ChangeRadiusY(Sender: TObject);
begin
  with Sender as TSpinEdit do begin
    RadiusY := (Sender as TSpinEdit).Value;
  end;
end;

  { TSelectTool }

constructor TSelectTool.Create;
begin
  Inherited;
  FIcon := 'imgs/Select.bmp';
end;

procedure TSelectTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor);
begin
  Figure := TFrame.Create;
  (Figure as TFrame).AddFirstPoint(X, Y);
  FFirstPoint := ScreenToWorld(X, Y);
end;

procedure TSelectTool.MouseMove(X, Y: Integer);
begin
  (Figure as TFrame).AddSecondPoint(X, Y);
  FSecondPoint := ScreenToWorld(X, Y);
end;

procedure TSelectTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer; Shift: TShiftState);
var
  i: integer;
begin
  (Figure as TFrame).AddSecondPoint(X, Y);
  FSecondPoint := ScreenToWorld(X, Y);
  if not (ssCtrl in Shift) then
    for i := High(Figures) downto Low(Figures) do
      Figures[i].Selected := false;
  for i := High(Figures) downto Low(Figures) do begin
    if Figures[i].IsIntersect(DoubleRect(FFirstPoint, FSecondPoint)) or
       Figures[i].IsPointInside(DoubleRect(FFirstPoint, FSecondPoint)) then
         Figures[i].Selected := not Figures[i].Selected;
  end;
  Figure := nil;
end;

procedure TSelectTool.InitParameters;
begin
  ParametersAvailable := False;
end;


initialization
  RegisterTool(THandTool.Create);
  RegisterTool(TPolyLineTool.Create);
  RegisterTool(TRectangleTool.Create);
  RegisterTool(TEllipseTool.Create);
  RegisterTool(TLineTool.Create);
  RegisterTool(TPolygonTool.Create);
  RegisterTool(TRoundRectTool.Create);
  RegisterTool(TZoomTool.Create);
  RegisterTool(TSelectTool.Create);
end.

