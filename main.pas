unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ComCtrls, StdCtrls, Buttons, Grids, PairSplitter, Spin, AboutForm,
  UFigures, UTools, math, LCLType, UScale, Types;

type

  { TMainScreen }

  TMainScreen = class(TForm)
    DrawGridVisible: TBitBtn;
    ColorDialog: TColorDialog;
    DrawGrid: TDrawGrid;
    BrushColorPanel: TPanel;
    BlackSquare: TPanel;
    FullExtent: TMenuItem;
    ScaleEdit: TFloatSpinEdit;
    ScrollBarRight: TScrollBar;
    ScrollBarBottom: TScrollBar;
    ScaleValue: TLabel;
    PenColorPanel: TPanel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    ExitBtn: TMenuItem;
    ClearScreen: TMenuItem;
    MenuItem3: TMenuItem;
    AboutBtn: TMenuItem;
    PaintBox: TPaintBox;
    ToolsPanel: TPanel;
    WidthChange: TTrackBar;
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure DrawGridVisibleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FullExtentClick(Sender: TObject);
    procedure PaintBoxMouseEnter(Sender: TObject);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ScaleEditChange(Sender: TObject);
    procedure ScrollBarBottomScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ScrollBarRightScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ToolsPanelMouseEnter(Sender: TObject);
    procedure ToolsPanelMouseLeave(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure SetScrollBarsParameters(ARect: TDoubleRect);
    procedure SetPaletteColors;
    procedure SetParametersPanel;
    procedure SetBtn;
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  paletteSize: TPoint = (X: 35; Y: 8);
    rows: integer = 8;
    cols: integer = 35;

var
  MainScreen: TMainScreen;
  WasMouseDown, MouseOnPaintBox: Boolean;
  Colors: array [0..34] of array [0..7] of integer;
  ToolParameters: TPanel;
  PenColor, BrushColor: TColor;

implementation

{$R *.lfm}

  { TMainScreen }



procedure TMainScreen.SetPaletteColors;
var
  i, j: integer;
begin
  for i := 0 to cols-1 do
    for j := 0 to rows-1  do
      Colors[i][j] := RGBToColor(
      floor(i/(cols-1)*255),
      floor(j/rows*255),
      floor((((cols-1)-i)*(rows-j)/(rows*(cols-1)))*255));
  for i := 0 to rows-1 do
    Colors[0][i] := RGBToColor(
    floor(i/(rows-1)*255),
    floor(i/(rows-1)*255),
    floor(i/(rows-1)*255));
  MainScreen.BrushColorPanel.Color := clWhite;
  MainScreen.PenColorPanel.Color := clBlack;
end;

procedure TMainScreen.SetParametersPanel;
begin
  ToolParameters := TPanel.Create(Self);
  with ToolParameters do begin
    Name := 'ToolParametrs';
    Parent := ToolsPanel;
    Caption := '';
    Width := 100;
    Left := 0;
    Top := ((Length(ToolRegistry) + 1) div 2) * 38 + 10;
    Color := clSilver;
  end;

end;

procedure TMainScreen.SetBtn;
var
  i: integer;
  ToolIcon: TBitmap;
begin
  for i := Low(ToolRegistry) to High(ToolRegistry) do begin
    ToolIcon := TBitmap.Create;
    ToolIcon.LoadFromFile(ToolRegistry[i].FIcon);
    With TBitBtn.Create(Self) do begin
      Parent  := ToolsPanel;
      Tag     := i;
      Width   := 46;
      Height  := 38;
      Top     := (i div 2) * Height + 5;
      Left    := (i mod 2) * Width + 5;
      Glyph   := ToolIcon;
      OnClick := @ToolClick;
    end;
  end;
end;

procedure TMainScreen.FormCreate(Sender: TObject);
begin
  InvalidateHandler := @Invalidate;
  SetPaletteColors;
  SetBtn;
  SetParametersPanel;
  ToolParameters.Visible := False;
end;

  { Btn }

procedure TMainScreen.ToolClick(Sender: TObject);
var
  b: TPanel;
begin
    CurrentTool := ToolRegistry[(Sender as TBitBtn).Tag];
    PaintBox.Cursor := crArrow;
    ToolParameters.Destroy;
    SetParametersPanel;
    CurrentTool.Init(ToolParameters);
    ToolParameters.Visible := CurrentTool.ParametersAvailable;
    MainScreen.Invalidate;
end;

procedure TMainScreen.DrawGridVisibleClick(Sender: TObject);
begin
  DrawGrid.Visible := not DrawGrid.Visible;
end;

procedure TMainScreen.ExitBtnClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainScreen.AboutBtnClick(Sender: TObject);
begin
  About.Show;
end;

procedure TMainScreen.FormPaint(Sender: TObject);
begin
  if (MainScreen.Height < 700) then
    MainScreen.Height := 700;
  if (MainScreen.Width < 300) then
    MainScreen.Width  := 300;
end;

procedure TMainScreen.FullExtentClick(Sender: TObject);
var
  ImageWidth, ImageHeigth, AScale: double;
  i: integer;

procedure Crutch;
begin
  if UFigures.Figures <> nil then begin
    ImageWidth := ImageCoords.Right - ImageCoords.Left + 15/Scale;
    ImageHeigth := ImageCoords.Bottom - ImageCoords.Top + 15/Scale;
    AScale := Min((PaintBox.Width - 15) / ImageWidth,
                 (PaintBox.Height - 15) / ImageHeigth);
    ScaleEdit.Value := AScale * 100;
    Scale := AScale;
    UScale.SetScreenCoords(ImageCoords.Left - 5/Scale,ImageCoords.Top - 5/Scale);
    PaintBox.Invalidate;
  end;
end;

begin
  for i := 1 to 3 do
    Crutch;
end;

  { DrawGrid }

procedure TMainScreen.DrawGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  DrawGrid.Canvas.Brush.Color := Colors[aCol][aRow];
end;

procedure TMainScreen.DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  aCol, aRow, i: Integer;
begin
  DrawGrid.MouseToCell(X, Y, aCol, aRow);
  if Button = mbLeft then begin
    PenColor := Colors[aCol][aRow];
    PenColorPanel.Color := PenColor;
    if CurrentTool is TSelectTool then;
      for i := Low(Figures) to High(Figures) do
        if Figures[i].Selected then
          Figures[i].FLineColor := PenColorPanel.Color;
  end;
  if Button = mbRight then begin
    BrushColor := Colors[aCol][aRow];
    BrushColorPanel.Color := BrushColor;
    if CurrentTool is TSelectTool then;
      for i := Low(Figures) to High(Figures) do
        if Figures[i].Selected then
          Figures[i].FBrushColor := BrushColorPanel.Color;
  end;
  Invalidate;
end;

procedure TMainScreen.DrawGridDblClick(Sender: TObject);
begin
  if ColorDialog.Execute then begin
    Colors[DrawGrid.Col][DrawGrid.Row] := ColorDialog.Color;
    PenColorPanel.Color := ColorDialog.Color;
    PenColor := ColorDialog.Color;
  end;
end;

  { PaintBox }

procedure TMainScreen.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  WasMouseDown := true;
  CurrentTool.MouseDown(X, Y, PenColorPanel.Color, BrushColorPanel.Color);
  MainScreen.Invalidate;
end;

procedure TMainScreen.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  If WasMouseDown then begin
    CurrentTool.MouseMove(X, Y);
  end;
  PaintBox.Invalidate;
end;

procedure TMainScreen.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  b: TDoubleRect;
begin
  WasMouseDown := false;
  CurrentTool.MouseUp(X, Y, PaintBox.Width, PaintBox.Height, Shift);
  if CurrentTool.GetFigure <> nil then begin
    CurrentTool.GetFigure.Draw(PaintBox.Canvas);
    UFigures.SaveActualFigure(CurrentTool.GetFigure);
  end;
  for i := 0 to High(Figures) do begin
      b := Figures[i].Bounds;
      if i = 0 then
        ImageCoords := b;
      with b do begin
        UpdateBorderCoords(Left, Top);
        UpdateBorderCoords(Right, Bottom);
      end;
  end;
  MainScreen.Invalidate;
end;

procedure TMainScreen.PaintBoxPaint(Sender: TObject);
var
  i : Integer;
begin
  For i := Low(UFigures.Figures) to High(UFigures.Figures) do begin
    UFigures.Figures[i].Draw(PaintBox.Canvas);
  end;
  if (WasMouseDown) and (CurrentTool.GetFigure <> nil) then
    CurrentTool.GetFigure.Draw(PaintBox.Canvas);
  ScrollBarBottom.Top := PaintBox.Height - ScrollBarBottom.Height;
  ScaleEdit.Value := Scale * 100;
  UScale.UpdateCanvasCoords(DoubleRect(ScreenToWorld(0, 0),
                          ScreenToWorld(PaintBox.Width, PaintBox.Height)));
  UScale.SetCoordsForBars(CanvasCoords, ImageCoords);
  if MouseOnPaintBox then
    SetScrollBarsParameters(CanvasCoords);
end;

procedure TMainScreen.SetScrollBarsParameters(ARect: TDoubleRect);
begin
  with ScrollBarBottom do begin
    Min := round(ARect.Left);
    Max := round(ARect.Right - PaintBox.Width/Scale);
    Position := round(ScreenToWorldX(0));
  end;
  with ScrollBarRight do begin
    Min := round(ARect.Top);
    Max := round(ARect.Bottom - PaintBox.Height/Scale);
    Position := round(ScreenToWorldY(0));
  end;
end;

procedure TMainScreen.ToolsPanelMouseEnter(Sender: TObject);
begin
  MouseOnPaintBox:=true;
end;

procedure TMainScreen.ToolsPanelMouseLeave(Sender: TObject);
begin
  MouseOnPaintBox:=false;
end;

procedure TMainScreen.PaintBoxMouseEnter(Sender: TObject);
begin
  MouseOnPaintBox:=true;
end;

procedure TMainScreen.PaintBoxMouseLeave(Sender: TObject);
begin
  MouseOnPaintBox:=False;
end;

  { Spins }

procedure TMainScreen.ScaleEditChange(Sender: TObject);
begin
  UScale.SetScale(ScaleEdit.Value / 100,
                  PaintBox.Width div 2,
                  PaintBox.Height div 2);
  PaintBox.Invalidate;
end;

procedure TMainScreen.ScrollBarBottomScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  MouseOnPaintBox:=false;
  SetScreenCoords(ScrollBarBottom.Position, ScrollBarRight.Position);
  PaintBox.Invalidate;
end;

procedure TMainScreen.ScrollBarRightScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  MouseOnPaintBox:=false;
  SetScreenCoords(ScrollBarBottom.Position, ScrollBarRight.Position);
  PaintBox.Invalidate;
end;

procedure TMainScreen.PaintBoxMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Scale/1.2 < 0.01 then SetScale(0.01, MousePos.X, MousePos.Y)
  else
    SetScale(Scale/1.2, MousePos.X, MousePos.Y);
  PaintBox.Invalidate;
end;

procedure TMainScreen.PaintBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Scale*1.2 > 20 then SetScale(20, MousePos.X, MousePos.Y)
  else
    SetScale(Scale*1.2, MousePos.X, MousePos.Y);
  PaintBox.Invalidate;
end;



initialization
  CurrentTool := ToolRegistry[0];
end.
