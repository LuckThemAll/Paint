unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, FPCanvas, Graphics;

const
  BufferLength = 100;

type

  THistoryBufer = array [1..BufferLength] of ArrayOfTFigure;

  THistory = class
    FPointer, AvailableUndo, AvailableRedo: integer;
    HistoryBuffer: THistoryBufer;
    UndoBtnAvailable, RedoBtnAvailable: Boolean;
    function NextPointer(APointer, ABufferLength: integer): integer;
    function PreviousPointer(APointer, ABufferLength: integer): integer;
    procedure SetInitialBufer;
    procedure SetEnableBtns;
    procedure SaveHistory;
    procedure Undo;
    procedure Redo;
  end;

var
  History: THistory;
  InvalidateHandler: procedure of Object;

implementation

  { TCircularBuffer }

function THistory.NextPointer(APointer, ABufferLength: integer): integer;
begin
  case (APointer + 1) > ABufferLength of
    True  : Result := 1;
    False : Result := APointer + 1;
  end;
end;

function THistory.PreviousPointer(APointer, ABufferLength: integer): integer;
begin
  case (APointer - 1) >= 1 of
    True  : Result := APointer - 1;
    False : Result := ABufferLength;
  end;
end;

procedure THistory.SetInitialBufer;
var
  i: Integer;
begin
  FPointer := 1;
  for i := 2 to BufferLength do
    HistoryBuffer[i] := Nil;
  AvailableRedo := 0;
  AvailableUndo := 0;
  UndoBtnAvailable := False;
  RedoBtnAvailable := False;
end;

procedure THistory.SetEnableBtns;
begin
  UndoBtnAvailable := History.AvailableUndo > 0;
  RedoBtnAvailable := History.AvailableRedo > 0;
end;

procedure THistory.SaveHistory;
var
  i: Integer;
begin
  FPointer := NextPointer(FPointer, BufferLength);
  if AvailableUndo <> BufferLength - 1 then
    Inc(AvailableUndo);
  AvailableRedo := 0;
  HistoryBuffer[FPointer] := Nil;
  for i := 0 to High(Figures) do begin;
    SetLength(HistoryBuffer[FPointer], Length(HistoryBuffer[FPointer]) + 1);
    HistoryBuffer[FPointer][i] := Figures[i].Copy;
  end;
  SetEnableBtns;
end;

procedure THistory.Undo;
var
  i: integer;
begin
  if AvailableUndo > 0 then begin
    FPointer := PreviousPointer(FPointer, BufferLength);
    Figures := Nil;
    SetLength(Figures, Length(HistoryBuffer[FPointer]));
    for i := 0 to High(HistoryBuffer[FPointer]) do
       Figures[i] := HistoryBuffer[FPointer][i];
    Dec(AvailableUndo);
    if AvailableRedo <> BufferLength - 1 then
      Inc(AvailableRedo);
  end;
  SetEnableBtns;
  InvalidateHandler;
end;

procedure THistory.Redo;
var
  i: Integer;
begin
  if AvailableRedo > 0 then begin;
    FPointer := NextPointer(FPointer, BufferLength);
    Figures := Nil;
    SetLength(Figures, Length(HistoryBuffer[FPointer]));
    for i := 0 to High(HistoryBuffer[FPointer]) do
      Figures[i] := HistoryBuffer[FPointer][i];
    Dec(AvailableRedo);
    if AvailableUndo <> BufferLength - 1 then
      Inc(AvailableUndo);
  end;
  SetEnableBtns;
  InvalidateHandler;
end;

initialization
History := THistory.Create;

end.

