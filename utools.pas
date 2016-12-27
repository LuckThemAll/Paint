unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, Graphics, UScale, math, ExtCtrls, Spin, StdCtrls,
   FPCanvas, Controls, UHistory;
type

  TFigureClass = class of TFigure;

  { TParameters }

  TParameterEditor = class
  public
    FLabel: TLabel;
    FComponent: TControl;
    constructor Create; virtual; abstract;
  end;

  ArrayOfParameters = array of TParameterEditor;

  TLineWidthParameter = class(TParameterEditor)
  public
    constructor Create; override;
    procedure onLineWidthChange(Sender: TObject);
  end;

  TLineStyleParameter = class(TParameterEditor)
  public
    constructor Create; override;
    procedure onLineStyleChange(Sender: TObject);
    procedure OnDrawLineStyleItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
  end;

  TBrushStyleParameter = class(TParameterEditor)
  public
    constructor Create; override;
    procedure onBrushStyleChange(Sender: TObject);
    procedure OnDrawBrushStyleItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
  end;

  TNumberOfAnglesParameter = class(TParameterEditor)
  public
    constructor Create; override;
    procedure onNumOfAnglesChange(Sender: TObject);
  end;

  TZoomParameter = class(TParameterEditor)
  public
    constructor Create; override;
    procedure onZoomChange(Sender: TObject);
  end;

  TRadiusXRoundRectParameter = class(TParameterEditor)
  public
    constructor Create; override;
    procedure onRadiusXChange(Sender: TObject);
  end;

  TRadiusYRoundRectParameter = class(TParameterEditor)
  public
    constructor Create; override;
    procedure onRadiusYChange(Sender: TObject);
  end;

  { TTools }

  TTool = class
  private
    Figure: TFigure;
    FPanel: TPanel;
    FigureClass: TFigureClass;
    ParameterEditors: ArrayOfParameters;
  public
    FIcon: String;
    ParametersAvailable: Boolean;
    function GetFigure: TFigure;
    function GetParameters: ArrayOfParameters; virtual; abstract;
    procedure AddParameter(AParameter: TParameterEditor);
    procedure Init(APanel: TPanel);
    procedure InitParameters; virtual; abstract;
    procedure ShowParameters;
    procedure UnselectAll;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  virtual; abstract;
    procedure MouseMove(X, Y: integer); virtual; abstract;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer;  Shift: TShiftState); virtual; abstract;
  end;

  THandTool = class(TTool)
  private
    FCurrentPoint: TDoublePoint;
  public
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters;           override;
  end;

  TZoomTool = class(TTool)
  private
    FFirstPoint, FSecondPoint: TDoublePoint;
    ZoomKoef: Double;
    ZoomParameter: String;
  public
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters           override;
  end;

  TLinesTool = class(TTool)
  private
    FLineWidth: integer;
    FLineStyle: TFPPenStyle;
  public
    function GetParameters: ArrayOfParameters; override;
  end;

  TFilledTool = class(TLinesTool)
  private
    FBrushStyle: TFPBrushStyle;
  public
    function GetParameters: ArrayOfParameters; override;
  end;

  TPolyLineTool = class(TLinesTool)
  public
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters           override;
    function GetParameters: ArrayOfParameters; override;
  end;

  TRectangleTool = class(TFilledTool)
  public
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters           override;
    function GetParameters: ArrayOfParameters; override;
  end;

  TPolygonTool = class(TFilledTool)
  private
    FNumberOfAngles: Integer;
  public
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters           override;
    function GetParameters: ArrayOfParameters; override;
  end;

  TEllipseTool = class(TFilledTool)
  public
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters;           override;
    function GetParameters: ArrayOfParameters; override;
  end;

  TLineTool = class(TLinesTool)
  public
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters;          override;
    function GetParameters: ArrayOfParameters; override;
  end;


  TRoundRectTool = class(TFilledTool)
  private
    RadiusX, RadiusY: integer;
  public
    constructor Create;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters;           override;
    function GetParameters: ArrayOfParameters; override;
  end;

  TSelectTool = class(TTool)
  private
    FFirstPoint, FSecondPoint: TDoublePoint;
  public
    constructor Create;
    function Equal(ADp: TDoublePoint; BDp: TDoublePoint):Boolean;
    procedure MouseDown(X, Y: integer;
      APenColor, ABrushColor: TColor);  override;
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer;
      AWidth, AHeight: Integer; Shift: TShiftState);        override;
    procedure InitParameters;           override;
    function CrossParameters: ArrayOfParameters;
  end;


var
  ToolRegistry: array of TTool;
  CurrentTool: TTool;
  FileWasChanged: boolean;

implementation

procedure RegisterTool(Tool: TTool; AFigureClass: TFigureClass);
begin
  SetLength(ToolRegistry, Length(ToolRegistry) + 1);
  ToolRegistry[High(ToolRegistry)] := Tool;
  ToolRegistry[High(ToolRegistry)].FigureClass := AFigureClass;
end;

  { TLineWidthParameter }

constructor TLineWidthParameter.Create;
begin
  FLabel := TLabel.Create(Nil);
  FLabel.Caption := 'Толщина линии';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 500;
    MinValue := 1;
    Value := 1;
    Width := 60;
    Left:= 0;
    OnChange := @onLineWidthChange;
  end;
end;

procedure TLineWidthParameter.onLineWidthChange(Sender: TObject);
var
  i: integer;
begin
  if not (CurrentTool is TSelectTool) then
    (CurrentTool as TLinesTool).FLineWidth := (Sender as TSpinEdit).Value
  else begin
    for i := 0 to High(Figures) do
      if Figures[i].Selected then begin
        (Figures[i] as TLinesFigure).FLineWidth := (Sender as TSpinEdit).Value;
        FileWasChanged := True;
      end;
    History.SaveHistory;
  end;
  InvalidateHandler;
end;

  { TLineStyleParameter }

constructor TLineStyleParameter.Create;
var
  i: integer;
begin
  FLabel := TLabel.Create(Nil);
  FLabel.Caption := 'Стиль линии';
  FComponent := TComboBox.Create(nil);
  with FComponent as TComboBox do begin
    AutoComplete := False;
    Width := 97;
    for i := 0 to 4 do Items.Add('');
    Style := csOwnerDrawFixed;
    ItemIndex := 0;
    Left:= 0;
    Font.Bold := True;
    Font.Size := 12;
    OnChange := @onLineStyleChange;
    OnDrawItem := @OnDrawLineStyleItem;
    ReadOnly := true;
  end;
end;

procedure TLineStyleParameter.OnDrawLineStyleItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas, ARect do begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Pen.Style := psClear;
    Pen.Color := clWhite;
    Top += 3;
    Left += 3;
    Right -= 3;
    Bottom -= 3;
    Rectangle(ARect);
    Pen.Style := TFPPenStyle(Index);
    Pen.Width := 3;
    Pen.Color := clBlack;
    Top += 6;
    Bottom := ARect.Top;
    Line(ARect);
  end;
end;

procedure TLineStyleParameter.onLineStyleChange(Sender: TObject);
var
  i: integer;
begin
  if not (CurrentTool is TSelectTool) then
    (CurrentTool as TLinesTool).FLineStyle := TFPPenStyle((Sender as TComboBox).ItemIndex)
  else begin
    for i := Low(Figures) to High(Figures) do
      if Figures[i].Selected then begin
        (Figures[i] as TLinesFigure).FLineStyle := TFPPenStyle((Sender as TComboBox).ItemIndex);
        FileWasChanged := True;
      end;
    History.SaveHistory;
  end;
  InvalidateHandler;
end;

  { TBrushStyleParameter }

constructor TBrushStyleParameter.Create;
var
  i: integer;
begin
  FLabel := TLabel.Create(Nil);
  FLabel.Caption := 'Стиль заливки';
  FComponent := TComboBox.Create(nil);
  with FComponent as TComboBox do begin
    Width := 97;
    for i := 0 to 7 do Items.Add('');
    Font.Size := 10;
    ItemIndex := 0;
    ReadOnly := true;
    Style := csOwnerDrawFixed;
    OnChange := @onBrushStyleChange;
    OnDrawItem := @OnDrawBrushStyleItem;
  end;
end;

procedure TBrushStyleParameter.OnDrawBrushStyleItem(
  Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas, ARect do begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Top += 2;
    Left += 2;
    Right -= 2;
    Bottom -= 2;
    Rectangle(ARect);
    if TFPBrushStyle(Index) = bsClear then begin
      Brush.Color := clWhite;
      Brush.Style := bsSolid;
    end
    else begin
      Brush.Style := TFPBrushStyle(Index);
      Brush.Color := clBlack;
    end;
    Pen.Color := clBlack;
    Rectangle(ARect);
  end;
end;

procedure TBrushStyleParameter.onBrushStyleChange(Sender: TObject);
var
  i: Integer;
begin
  if not (CurrentTool is TSelectTool) then
    (CurrentTool as TFilledTool).FBrushStyle := TFPBrushStyle((Sender as TComboBox).ItemIndex)
  else begin
    for i := Low(Figures) to High(Figures) do
      if Figures[i].Selected then begin
        (Figures[i] as TFilledFigures).FBrushStyle := TFPBrushStyle((Sender as TComboBox).ItemIndex);
        FileWasChanged := True;
      end;
    History.SaveHistory;
  end;
  InvalidateHandler;
end;

  { TNumberOfAnglesParameter }

constructor TNumberOfAnglesParameter.Create;
begin
  FLabel := TLabel.Create(Nil);
  FLabel.Caption := 'Количество углов';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 15;
    MinValue := 3;
    Value := 3;
    Width := 60;
    OnChange := @onNumOfAnglesChange;
  end;
end;

procedure TNumberOfAnglesParameter.onNumOfAnglesChange(Sender: TObject);
var
  i: integer;
begin
  if not (CurrentTool is TSelectTool) then
    (CurrentTool as TPolygonTool).FNumberOfAngles := (Sender as TSpinEdit).Value
  else begin
    for i := Low(Figures) to High(Figures) do
      if Figures[i].Selected then begin
        (Figures[i] as TPolygon).NumberOfAngles := (Sender as TSpinEdit).Value;
        FileWasChanged := True;
      end;
    History.SaveHistory;
  end;
  InvalidateHandler;
end;

  { TRadiusXRoundRectParameter }

constructor TRadiusXRoundRectParameter.Create;
begin
  FLabel := TLabel.Create(Nil);
  FLabel.Caption := 'Radius X';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 100;
    MinValue := 0;
    Value := 10;
    Width := 60;
    OnChange := @onRadiusXChange;
  end;
end;

procedure TRadiusXRoundRectParameter.onRadiusXChange(Sender: TObject);
var
  i: integer;
begin
  if not (CurrentTool is TSelectTool) then
      (CurrentTool as TRoundRectTool).RadiusX := (Sender as TSpinEdit).Value
    else begin
      for i := Low(Figures) to High(Figures) do
        if Figures[i].Selected then begin
          (Figures[i] as TRoundRect).RadiusX := (Sender as TSpinEdit).Value;
          FileWasChanged := True;
        end;
      History.SaveHistory;
    end;
  InvalidateHandler;
end;

  { TRadiusYRoundRectParameter }

constructor TRadiusYRoundRectParameter.Create;
begin
  FLabel := TLabel.Create(Nil);
  FLabel.Caption := 'Radius Y';
  FComponent := TSpinEdit.Create(nil);
  with FComponent as TSpinEdit do begin
    MaxValue := 100;
    MinValue := 0;
    Value := 10;
    Width := 60;
    OnChange := @onRadiusYChange;
  end;
end;

procedure TRadiusYRoundRectParameter.onRadiusYChange(Sender: TObject);
var
  i: integer;
begin
  if not (CurrentTool is TSelectTool) then
    (CurrentTool as TRoundRectTool).RadiusY := (Sender as TSpinEdit).Value
    else begin
      for i := Low(Figures) to High(Figures) do
        if Figures[i].Selected then begin
          (Figures[i] as TRoundRect).RadiusY := (Sender as TSpinEdit).Value;
          FileWasChanged := True;
        end;
      History.SaveHistory;
    end;
  InvalidateHandler;
end;

  { TZoomParameter }

constructor TZoomParameter.Create;
begin
  FLabel := TLabel.Create(Nil);
  FLabel.Caption := 'Вид лупы';
  FComponent := TComboBox.Create(nil);
  with FComponent as TComboBox do begin
    Items.Add('Увеличить');
    Items.Add('Уменьшить');
    Items.Add('Приблизить область');
    AutoComplete := False;
    ItemIndex := 0;
    Width := 97;
    Left:= 0;
    OnChange := @onZoomChange;
    ReadOnly := true;
  end;
end;

procedure TZoomParameter.onZoomChange(Sender: TObject);
begin
    (CurrentTool as TZoomTool).ZoomParameter := (Sender as TComboBox).Text;
end;

  { TTool }

procedure TTool.AddParameter(AParameter: TParameterEditor);
begin
  SetLength(ParameterEditors, Length(ParameterEditors) + 1);
  ParameterEditors[High(ParameterEditors)] := AParameter;
end;

procedure TTool.Init(APanel: TPanel);
var
  i: Integer;
begin
  ParameterEditors := nil;
  FPanel := APanel;
  InitParameters;
  ShowParameters;
end;

procedure TTool.ShowParameters;
var
  i: integer;
  Delta: integer = 5;
begin
  for i := FPanel.ControlCount-1 downto 0 do (FPanel.Controls[i]).Free;
  if Length(ParameterEditors) <> 0 then FPanel.Height := Delta
  else
    FPanel.Height := 0;
  for i := 0 to High(ParameterEditors) do begin
    with ParameterEditors[i] do begin
      FLabel.Top        := i * 40;
      FLabel.Left       := 2;
      FLabel.Parent     := FPanel;
      FComponent.Top    := i * 40 + FLabel.ClientHeight;
      FComponent.Left   := 2;
      FComponent.Parent := FPanel;
    end;
  end;
  for i := Low(ParameterEditors) to High(ParameterEditors) do begin
    FPanel.Height :=(
      FPanel.Height +
      ParameterEditors[i].FLabel.Height +
      ParameterEditors[i].FComponent.Height);
  end
end;

function TTool.GetFigure: TFigure;
begin
  Result := Figure;
end;

procedure TTool.UnselectAll;
var
  i: integer;
begin
  for i := High(Figures) downto Low(Figures) do
      Figures[i].Selected := false;
end;

  { TLinesTool }

function TLinesTool.GetParameters: ArrayOfParameters;
begin
  SetLength(Result, 2);
  Result[0] := TLineWidthParameter.Create;
  Result[1] := TLineStyleParameter.Create;
end;

  { TFilldTool }

function TFilledTool.GetParameters: ArrayOfParameters;
begin
  Inherited;
  Result := Inherited;
  SetLength(Result, Length(Result) + 1);
  Result[High(Result)] := TBrushStyleParameter.Create;
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
  ParametersAvailable := True;
  AddParameter(TZoomParameter.Create);
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
  if Selected then begin
    FileWasChanged := True;
    for i := Low(Figures) to High(Figures) do
      if Figures[i].Selected then
        Figures[i].Move(NewWorldCoords);
  end;
  if not Selected then
    ChangeScreenCoords(
      FCurrentPoint.X - ScreenToWorldX(X),
      FCurrentPoint.Y - ScreenToWorldY(Y))
  else
    FCurrentPoint := ScreenToWorld(X, Y);
end;

procedure THandTool.MouseUp(X, Y: integer; AWidth, AHeight: Integer; Shift: TShiftState);
var
  i: Integer;
begin
  for i := Low(Figures) to High(Figures) do
    if Figures[i].Selected then begin
      History.SaveHistory;
      Break;
    end;
end;

procedure THandTool.InitParameters;
begin
  ParametersAvailable := false;
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
  if FLineWidth = 0 then FLineWidth := 1;
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
  AddParameter(TLineWidthParameter.Create);
  AddParameter(TLineStyleParameter.Create);
end;

function TPolylineTool.GetParameters: ArrayOfParameters;
begin
  Inherited;
  Result := Inherited;
end;

  { TRectangleTool }

constructor TRectangleTool.Create;
begin
  Inherited;
  FIcon := 'imgs/Rectangle.bmp';
end;

procedure TRectangleTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor);
begin
  UnselectAll;
  if FLineWidth = 0 then FLineWidth := 1;
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
  AddParameter(TLineWidthParameter.Create);
  AddParameter(TLineStyleParameter.Create);
  AddParameter(TBrushStyleParameter.Create);
end;

function TRectangleTool.GetParameters: ArrayOfParameters;
begin
  Inherited;
  Result := Inherited;
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
  UnselectAll;
  if FLineWidth = 0 then FLineWidth := 1;
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
  AddParameter(TLineWidthParameter.Create);
  AddParameter(TLineStyleParameter.Create);
  AddParameter(TBrushStyleParameter.Create);
  AddParameter(TNumberOfAnglesParameter.Create);
end;

function TPolygonTool.GetParameters: ArrayOfParameters;
begin
  Inherited;
  Result := Inherited;
  SetLength(Result, Length(Result) + 1);
  Result[High(Result)] := TNumberOfAnglesParameter.Create;
end;

  { TEllipseTool }

constructor TEllipseTool.Create;
begin
  Inherited;
  FIcon := 'imgs/Ellipse.bmp';
end;

procedure TEllipseTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor);
begin
  UnselectAll;
  if FLineWidth = 0 then FLineWidth := 1;
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
  AddParameter(TLineWidthParameter.Create);
  AddParameter(TLineStyleParameter.Create);
  AddParameter(TBrushStyleParameter.Create);
end;

function TEllipseTool.GetParameters: ArrayOfParameters;
begin
  Inherited;
  Result := Inherited;
end;

  { TLineTool }

constructor TLineTool.Create;
begin
  Inherited;
  FIcon := 'imgs/Line.bmp';
end;

procedure TLineTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor);
begin
  UnselectAll;
  if FLineWidth = 0 then FLineWidth := 1;
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
  AddParameter(TLineWidthParameter.Create);
  AddParameter(TLineStyleParameter.Create);
end;

function TLineTool.GetParameters: ArrayOfParameters;
begin
  Inherited;
  Result := Inherited;
end;

{ TRounRectTool }

constructor TRoundRectTool.Create;
begin
  Inherited;
  FIcon := 'imgs/RoundRect.bmp';
  RadiusX := 10;
  RadiusY := 10;
end;

procedure TRoundRectTool.MouseDown(X, Y: Integer; APenColor, ABrushColor: TColor);
begin
  UnselectAll;
  if FLineWidth = 0 then FLineWidth := 1;
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
  AddParameter(TLineWidthParameter.Create);
  AddParameter(TLineStyleParameter.Create);
  AddParameter(TBrushStyleParameter.Create);
  AddParameter(TRadiusXRoundRectParameter.Create);
  AddParameter(TRadiusYRoundRectParameter.Create);
end;

function TRoundRectTool.GetParameters: ArrayOfParameters;
begin
  Inherited;
  Result := Inherited;
  SetLength(Result, Length(Result) + 2);
  Result[High(Result) - 1] := TRadiusXRoundRectParameter.Create;
  Result[High(Result)] := TRadiusYRoundRectParameter.Create;
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

procedure TSelectTool.MouseUp(X, Y: Integer; AWidth, AHeight: Integer;
  Shift: TShiftState);

function isFigureSelected(k: integer):Boolean;
begin
  Result := Figures[k].IsIntersect(DoubleRect(FFirstPoint, FSecondPoint)) or
            Figures[k].IsPointInside(FFirstPoint)
end;

procedure SelectFigure(k: integer);
begin
  Figures[k].Selected := not Figures[k].Selected;
end;

var
  i: integer;
begin
  (Figure as TFrame).AddSecondPoint(X, Y);
  FSecondPoint := ScreenToWorld(X, Y);
  if not (ssCtrl in Shift) then
    UnselectAll;
  if Equal(FFirstPoint, FSecondPoint) then begin
  for i := High(Figures) downto Low(Figures) do
    if isFigureSelected(i) then begin
         SelectFigure(i);
         Break;
       end;
  end
  else
    for i := High(Figures) downto Low(Figures) do
      if isFigureSelected(i) then
           SelectFigure(i);
  Figure := nil;
  ParameterEditors := CrossParameters;
  ShowParameters;
  InvalidateHandler;
end;

procedure TSelectTool.InitParameters;
begin
  ParametersAvailable := True;
end;

function TSelectTool.Equal(ADp: TDoublePoint; BDp: TDoublePoint):Boolean;
begin
  Result := (ADp.X = BDp.X) and (ADp.Y = BDp.Y);
end;

function TSelectTool.CrossParameters: ArrayOfParameters;
var
  i, j, h, k: Integer;
  NeedDelet: Boolean;
  Prop: ArrayOfParameters;
  CommonParams: ArrayOfParameters;
begin
  CommonParams := nil;
  Prop := nil;
  for i := 0 to High(Figures) do
    if Figures[i].Selected then begin
      for j := 0 to High(ToolRegistry) do
        if ToolRegistry[j].FigureClass = Figures[i].ClassType then begin
            Prop := ToolRegistry[j].GetParameters;
            Break;
        end;
      if Length(CommonParams) = 0 then begin
        SetLength(CommonParams, Length(Prop));
        CommonParams := Prop;
      end
      else begin
        for k := 0 to High(CommonParams) do begin
          NeedDelet := true;
          for h := 0 to High(Prop) do
            if (CommonParams[k] <> nil) and (CommonParams[k].ClassName = Prop[h].ClassName) then
            NeedDelet := false;
          if NeedDelet then
            CommonParams[k] := nil;
        end;
      end;
    end;
  for i := 0 to High(CommonParams) do
    if CommonParams[i] <> nil then begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := CommonParams[i];
    end;
end;

initialization
  RegisterTool(THandTool.Create, Nil);
  RegisterTool(TPolyLineTool.Create, TPolyLine);
  RegisterTool(TRectangleTool.Create, TRectangle);
  RegisterTool(TEllipseTool.Create, TEllipse);
  RegisterTool(TLineTool.Create, TLine);
  RegisterTool(TPolygonTool.Create, TPolygon);
  RegisterTool(TRoundRectTool.Create, TRoundRect);
  RegisterTool(TZoomTool.Create, Nil);
  RegisterTool(TSelectTool.Create, Nil);
end.

