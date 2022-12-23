unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Script.Calc, TypInfo;

type
  TMainForm = class(TForm)
    TreeView1: TTreeView;
    ButtonReturn: TButton;
    EditReturnValue: TEdit;
    MemoError: TMemo;
    LabelAST: TLabel;
    LabelError: TLabel;
    MemoAST: TMemo;
    MemoScript: TMemo;
    procedure FormShow(Sender: TObject);
    procedure ButtonReturnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CalcAST(const ARoot: TExpression);
    procedure CalcAST_DFM(const ARoot: TExpression);
    procedure CalcExpression(const Value: String);
    procedure RunScript(const Value: String);
  end;

var
  MainForm: TMainForm;


implementation

{$R *.dfm}

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Калькулятор (Тестовое задание от ДНК)';
end;

procedure TMainForm.RunScript(const Value: String);
var
  xScript: TScript;
begin
  xScript := TScript.Create;
  try
    xScript.Assign(MemoScript.Lines);
    xScript.Compile;
    EditReturnValue.Text := VarToStr(xScript.Execute);
  finally
    FreeAndNil(xScript);
  end;
end;

procedure TMainForm.ButtonReturnClick(Sender: TObject);
var
  xS: String;
begin
  TreeView1.Items.Clear;
  MemoAST.Lines.Clear;
  xS := MemoScript.Text;
  if not xS.IsEmpty then
  begin
    RunScript(xS);
//    CalcExpression(xS);
//    if ComboBoxExpression.Items.IndexOf(xS) < 0 then
//    begin
//      ComboBoxExpression.Items.Add(xS);
//      ComboBoxExpression.Items.SaveToFile('expression.txt');
//    end;
  end;
end;

procedure TMainForm.CalcAST(const ARoot: TExpression);

  procedure SetNodeTree(ANode: TTreeNode; ARL: String; AExpression: TExpression);
  var
    xNode: TTreeNode;
  begin
    xNode := TreeView1.Items.AddChild(ANode,ARL + ' ' + AExpression.Token.ToString + ' res: ' + FloatToStr(AExpression.Eval));
    if Assigned(AExpression.Left) then
      SetNodeTree(xNode,'L',AExpression.Left);
    if Assigned(AExpression.Rigth) then
      SetNodeTree(xNode,'R',AExpression.Rigth);
  end;

  procedure SetTree(AExpression: TExpression);
  var
    xNode: TTreeNode;
  begin
    xNode := TreeView1.Items.AddChild(nil,AExpression.Token.ToString  + ' res: ' + FloatToStr(AExpression.Eval));
    if Assigned(AExpression.Left) then
      SetNodeTree(xNode,'L',AExpression.Left);
    if Assigned(AExpression.Rigth) then
      SetNodeTree(xNode,'R',AExpression.Rigth);
  end;

begin
  TreeView1.Items.Clear;
  SetTree(ARoot);
  TreeView1.FullExpand;
end;

procedure TMainForm.CalcAST_DFM(const ARoot: TExpression);
const
  STEP_TAB = 4;

  function _Tab(const ACount: Integer): String;
  var
    xS: String;
    i: Integer;
  begin
    xS := '';
    for i := 1 to ACount do
      xS := xS + ' ';
    Result := xS;
  end;

  procedure _Gen_Token(AToken: TToken; const ATab: Integer; ADest: TStrings);
  var
    xTab: String;
  begin
    xTab := _Tab(ATab);
    ADest.Add(xTab + 'Value=' + AToken.Value);
    ADest.Add(xTab + 'Priority=' + AToken.Priority.ToString);
    ADest.Add(TypInfo.GetEnumName(TypeInfo(TTypeToken),Integer(AToken.TypeToken)));
//    case AToken.TypeToken of
//      ttNull: ADest.Add(xTab + 'TypeToken=ttNull');
//      ttNumber: ADest.Add(xTab + 'TypeToken=ttNumber');
//      ttOperator: ADest.Add(xTab + 'TypeToken=ttOperator');
//    end;
  end;

  procedure _Gen_DFM(AExpression: TExpression; const ATab: Integer; ADest: TStrings);
  var
    xTab: String;
  begin
    xTab := _Tab(ATab);
    ADest.Add(xTab + 'expression: name = "' + AExpression.Name + '"');
    _Gen_Token(AExpression.Token,ATab + STEP_TAB,ADest);
    if Assigned(AExpression.Left) then
      _Gen_DFM(AExpression.Left,ATab + STEP_TAB,ADest);
    if Assigned(AExpression.Rigth) then
      _Gen_DFM(AExpression.Rigth,ATab + STEP_TAB,ADest);
    ADest.Add(xTab + 'end');
  end;

begin
  _Gen_DFM(ARoot,0,MemoAST.Lines);
end;

procedure TMainForm.CalcExpression(const Value: String);
var
  xCalc: TCalc;
begin
  try
    xCalc := TCalc.Create;
    try
      xCalc.Value := Value;
      xCalc.Compile;
      EditReturnValue.Text := FloatToStr(xCalc.ReturnValue);
      {todo: нужно другое место}
      if Assigned(xCalc.Root) then
      begin
        CalcAST(xCalc.Root);
        CalcAST_DFM(xCalc.Root);
      end;
    finally
      FreeAndNil(xCalc);
    end;
  except
    on E: Exception do
    begin
      EditReturnValue.Text := '';
      MemoError.Lines.Add('## #########################################');
      MemoError.Lines.Add(E.Message);
    end;
  end;
end;

end.
