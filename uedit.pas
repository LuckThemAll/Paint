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
  end;



  var
    Edit: TEdit;

implementation

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

initialization
  Edit := TEdit.Create;
end.

