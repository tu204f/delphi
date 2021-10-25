unit UnitMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo: TMemo;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Lb.StringReplace;

procedure TMainForm.Button1Click(Sender: TObject);
var
  before: String;
  xS: String;
  OldPattern, NewPattern: String;
begin
  // --------------------------------------------------------------------------
  before := 'This is aaaab way to live AAAAB big life';
  Memo.Lines.Add(before);
  Memo.Lines.Add('');


  NewPattern := ' THEN ';

  OldPattern := ' aaaab ';
  xS := GetStringReplace(before,OldPattern,NewPattern,-1,True,True);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
  // --------------------------------------------------------------------------
  OldPattern := ' aaaab ';
  xS := GetStringReplace(before,OldPattern,NewPattern,-1,True,False);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
  // --------------------------------------------------------------------------
  OldPattern := ' AAAAB ';
  xS := GetStringReplace(before,OldPattern,NewPattern,-1,True,False);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
  // --------------------------------------------------------------------------
  OldPattern := ' aaaab ';
  xS := GetStringReplace(before,OldPattern,NewPattern,-1,False,True);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
  // --------------------------------------------------------------------------
  OldPattern := ' aaaab ';
  xS := GetStringReplace(before,OldPattern,NewPattern,-1,False,False);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
  // --------------------------------------------------------------------------
  OldPattern := ' AAAAB ';
  xS := GetStringReplace(before,OldPattern,NewPattern,-1,False,False);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
  // --------------------------------------------------------------------------
  // --------------------------------------------------------------------------
  OldPattern := ' aaaab ';
  xS := GetStringReplace(before,OldPattern,NewPattern,0,True,True);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
  // --------------------------------------------------------------------------
  OldPattern := ' aaaab ';
  xS := GetStringReplace(before,OldPattern,NewPattern,0,True,False);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
  // --------------------------------------------------------------------------
  OldPattern := ' AAAAB ';
  xS := GetStringReplace(before,OldPattern,NewPattern,0,True,False);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
  // --------------------------------------------------------------------------
  OldPattern := ' aaaab ';
  xS := GetStringReplace(before,OldPattern,NewPattern,0,False,True);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
  // --------------------------------------------------------------------------
  OldPattern := ' aaaab ';
  xS := GetStringReplace(before,OldPattern,NewPattern,0,False,False);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
  // --------------------------------------------------------------------------
  OldPattern := ' AAAAB ';
  xS := GetStringReplace(before,OldPattern,NewPattern,0,False,False);
  Memo.Lines.Add('[' + OldPattern + '] ' + xS);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //
  // StringReplace()
end;

end.
