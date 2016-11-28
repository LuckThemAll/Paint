unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TAbout }

  TAbout = class(TForm)
    Author: TLabel;
    Group: TLabel;
    DateOfCraetion: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  About: TAbout;

implementation

{$R *.lfm}

{ TAbout }

end.

