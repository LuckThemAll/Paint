unit UEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UTools, UFigures;

type

  TEdit = class
    CopiedFigures: ArrayOfTFigure;
    procedure CopyFigure; virtual;
    procedure InsertFigures;
    procedure UpLayer;
    procedure DownLayer;
    procedure MoveBack;
    procedure MoveFront;
    procedure Cut;
    procedure Delete;
  end;


  var
    Edit: TEdit;

implementation

procedure CompactFigures;
var
  i, j: integer;
begin
  for i := Low(Figures) to High(Figures) do
    if Figures[i] = nil then
      for j := i to High(Figures) - 1 do begin
        if Figures[j+1] = Nil then begin
          Figures[j] := Nil;
          Figures[j].Free;
        end
        else
          Figures[j] := Figures[j+1].Copy;
      end;
  SetLength(Figures, Length(Figures) - 1);
end;

procedure TEdit.CopyFigure;
var
  i: Integer;
  Counter: Integer = 0;
begin
  for i := Low(Figures) to High(Figures) do
    if Figures[i].Selected then begin
      Inc(Counter);
      SetLength(CopiedFigures, Counter);
      CopiedFigures[High(CopiedFigures)] := Figures[i].Copy;
    end;
end;

procedure TEdit.InsertFigures;
var
  i, HighFigures: integer;
  Counter: Integer = 0;
begin
  if CopiedFigures <> Nil then begin
    HighFigures := High(Figures);
    SetLength(Figures, Length(Figures) + Length(CopiedFigures));
    for i := (HighFigures + 1) to (HighFigures + Length(CopiedFigures)) do begin
      Figures[i] := CopiedFigures[Counter].Copy;
      Figures[i].Selected := True;
      Inc(Counter);
    end;
  end;
end;

procedure TEdit.Cut;
var
  i: integer;
  Counter: Integer = 0;

begin
  for i := Low(Figures) to High(Figures) do
    if Figures[i].Selected then begin
      Inc(Counter);
      SetLength(CopiedFigures, Counter);
      CopiedFigures[High(CopiedFigures)] := Figures[i].Copy;
      Figures[i] := Nil;
      Figures[i].Free;
    end;
  for i := 1 to Length(CopiedFigures) do
    CompactFigures;
end;

procedure TEdit.Delete;
var
  i: integer;
  Counter: Integer = 0;

begin
  for i := Low(Figures) to High(Figures) do
    if Figures[i].Selected then begin
      Inc(Counter);
      Figures[i] := Nil;
      Figures[i].Free;
    end;
  for i := 1 to Counter do
    CompactFigures;
end;

procedure SwapFigures(n, v: integer);
var
  f: TFigure;
begin
  case v of
  1: if not Figures[n+1].Selected then begin
         f := Figures[n].Copy;
         Figures[n].Free;
         Figures[n] := Figures[n+1].Copy;
         Figures[n+1].Free;
         Figures[n+1] := f.Copy;
         f.Free;
       end;
  0: if not Figures[n-1].Selected then begin
         f := Figures[n].Copy;
         Figures[n].Free;
         Figures[n] := Figures[n-1].Copy;
         Figures[n-1].Free;
         Figures[n-1] := f.Copy;
         f.Free;
       end;
  end;
end;

procedure TEdit.UpLayer;
var
  i: integer;
begin
  i := High(Figures)-1;
  while i > Low(Figures)-1 do begin
    if Figures[i].Selected then begin
      SwapFigures(i, 1);
      Figures[i+1].Selected := True;
      Dec(i);
    end;
    Dec(i);
  end;
end;

procedure TEdit.DownLayer;
var
  i: integer;
begin
  i := Low(Figures)+1;
  while i < High(Figures)+1 do begin
    if Figures[i].Selected then begin
      SwapFigures(i, 0);
      Figures[i-1].Selected := True;
      Inc(i);
    end;
    Inc(i);
  end;
end;

procedure TEdit.MoveBack;
var
  i: integer;
begin
  for i := 0 to High(Figures) do
    Edit.DownLayer;
end;

procedure TEdit.MoveFront;
var
  i: integer;
begin
  for i := 0 to High(Figures) do
    Edit.UpLayer;
end;

initialization
  Edit := TEdit.Create;
end.

