unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, FPCanvas, Graphics;

const
   BufferLength = 30;

type

  ArrStrArr = array [1..BufferLength] of ArrayOfTFigure;

  TCircularBuffer = class
    FPointer, AvaibleUndo, AvaibleRedo: integer;
    HistoryBuffer: ArrStrArr;
    procedure AddToBufer;
    procedure Undo;
    procedure Redo;
  end;

var
  History: TCircularBuffer;
  InvalidateHandler: procedure of Object;
  m: Boolean;

implementation

  { TCircularBuffer }

procedure TCircularBuffer.AddToBufer;
var
  i: Integer;
begin
  if AvaibleUndo < BufferLength then
    Inc(AvaibleUndo);
  if (FPointer + 1) > BufferLength then
    FPointer := 1
  else
    Inc(FPointer);
  HistoryBuffer[FPointer] := Figures;
  AvaibleRedo := 0;
end;

procedure TCircularBuffer.Undo;
var
  i: integer;
begin
  Figures := Nil;
  if AvaibleUndo > 0 then begin
    Dec(AvaibleUndo);
    Inc(AvaibleRedo);
    if (FPointer - 1) < 1 then
      FPointer := BufferLength
    else
      Dec(FPointer);
    Figures := HistoryBuffer[FPointer];
  end;
  InvalidateHandler;
end;

procedure TCircularBuffer.Redo;
begin
  if (AvaibleRedo > 0) then
    begin
      inc(FPointer);
      if (AvaibleUndo < BufferLength) then
        inc(AvaibleUndo);
      dec(AvaibleRedo);
      if FPointer > BufferLength then
        FPointer := 1;
      Figures := HistoryBuffer[FPointer];
    end;
  InvalidateHandler;
end;

initialization
History := TCircularBuffer.Create;

end.

