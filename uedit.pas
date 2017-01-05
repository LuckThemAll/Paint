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
    procedure Cut;
    procedure Delete;
  end;


  var
    Edit: TEdit;

implementation

procedure ShiftFigures;
var
  i, j: integer;
begin
  for i := Low(Figures) to High(Figures) do
    if Figures[i] = nil then
      for j := i to High(Figures) - 1 do begin
        if Figures[j+1] = Nil then
          Figures[j] := Nil
        else
          Figures[j] := Figures[j+1].Copy;
      end;
  SetLength(Figures, Length(Figures) - 1);
end;

procedure TEdit.CopyFigure;
var
  i: Integer;
  Counter: Integer;
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
  i, j, k, HighFigures: integer;
  Counter: Integer = 0;
begin
  if CopiedFigures <> Nil then begin
    HighFigures := High(Figures);
    SetLength(Figures, Length(Figures) + Length(CopiedFigures));
    j := High(Figures);
    k := Length(Figures);
    j := (HighFigures + Length(CopiedFigures));
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
    end;
  for i := 1 to Length(CopiedFigures) do
    ShiftFigures;
end;

procedure TEdit.Delete;
var
  i: integer;
  Counter: Integer = 0;

begin
  for i := Low(Figures) to High(Figures) do
    if Figures[i].Selected then begin
      Inc(Counter);
      Figures[i].Selected := False;
      Figures[i] := Nil;
    end;
  for i := 1 to Counter do
    ShiftFigures;
end;

initialization
  Edit := TEdit.Create;
end.

