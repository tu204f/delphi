(*******************************************************************************
  Пример редактирование
*******************************************************************************)
unit UnitDocumentForm;

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
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Grids,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Lb.Doc.DB;

type
  TDocumentForm = class(TForm)
    ButtonWrite: TButton;
    SGrid: TStringGrid;
    LabeledEdit1: TLabeledEdit;
    ButtonAddField1: TButton;
    Label1: TLabel;
    Memo: TMemo;
    ButtonAddField2: TButton;
    DateTimePicker1: TDateTimePicker;
    DateTimePicker2: TDateTimePicker;
    ButtonAddField3: TButton;
    ButtonAddField4: TButton;
    DateTimePicker3: TDateTimePicker;
    DateTimePicker4: TDateTimePicker;
    ButtonAddField5: TButton;
    LabeledEdit2: TLabeledEdit;
    ButtonAddField6: TButton;
    LabeledEdit3: TLabeledEdit;
    ButtonAddField7: TButton;
    ButtonAddField8: TButton;
    CheckBox1: TCheckBox;
    LabeledEdit4: TLabeledEdit;
    ButtonAddField9: TButton;
    UpDown1: TUpDown;
    LabeledEdit5: TLabeledEdit;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure ButtonWriteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonAddField1Click(Sender: TObject);
    procedure ButtonAddField2Click(Sender: TObject);
    procedure ButtonAddField3Click(Sender: TObject);
    procedure ButtonAddField4Click(Sender: TObject);
    procedure ButtonAddField5Click(Sender: TObject);
    procedure ButtonAddField6Click(Sender: TObject);
    procedure ButtonAddField7Click(Sender: TObject);
    procedure ButtonAddField8Click(Sender: TObject);
    procedure ButtonAddField9Click(Sender: TObject);
  private
    FDocument: TDocument;
    procedure SetDocument(const Value: TDocument);
    procedure SetDocumentShowGrid;
  public
    property Document: TDocument write SetDocument;
  end;

procedure SetDocumentForm(const ADocument: TDocument);

implementation

uses
  Lb.Doc.DB.Params;

{$R *.dfm}

procedure SetDocumentForm(const ADocument: TDocument);
var
  xDocumentForm: TDocumentForm;
begin
  xDocumentForm := TDocumentForm.Create(nil);
  try
    xDocumentForm.Document := ADocument;
    if xDocumentForm.ShowModal = mrOk then
      ///
  finally
    FreeAndNil(xDocumentForm);
  end;
end;

procedure TDocumentForm.FormCreate(Sender: TObject);
begin
  Self.Caption := 'Документ';
end;

procedure TDocumentForm.FormShow(Sender: TObject);
begin
  SGrid.Cells[0,0] := 'Ключ';
  SGrid.Cells[1,0] := 'Значение';
  SGrid.Cells[2,0] := 'type_table';
  SGrid.Cells[3,0] := 'type_field';
  FDocument.Read;
  SetDocumentShowGrid;
end;

procedure TDocumentForm.SetDocument(const Value: TDocument);
begin
  FDocument := Value;
  Self.Caption :=  'Документ ' + FDocument.ID;
end;

procedure TDocumentForm.SetDocumentShowGrid;
var
  i, Count: Integer;
  xValue: TParams.TValue;
begin
  Count := FDocument.Params.Count;
  if Count > 0 then
  begin
    SGrid.RowCount := Count + 1;
    for i := 0 to Count - 1 do
    begin
      xValue := FDocument.Params.Items[i];
      SGrid.Cells[0,i + 1] := xValue.Name;
      SGrid.Cells[1,i + 1] := xValue.ValueString;
      SGrid.Cells[2,i + 1] := '';
      SGrid.Cells[3,i + 1] := TParams.StrToTypeValue(xValue.TypeValue);
    end;
  end
  else
  begin
    SGrid.ColCount := 2;
    SGrid.Rows[1].Clear;
  end;
  LabeledEdit5.Text := 'field_' + IntToStr(Count + 1);
end;

procedure TDocumentForm.ButtonWriteClick(Sender: TObject);
begin
  FDocument.Write;
  Self.ModalResult := mrOk;
end;

procedure TDocumentForm.ButtonAddField1Click(Sender: TObject);
var
  xName: String;
begin
  xName := LabeledEdit5.Text;
  FDocument.Params.ValueByName[xName].AsString := LabeledEdit1.Text;
  SetDocumentShowGrid;
end;

procedure TDocumentForm.ButtonAddField2Click(Sender: TObject);
var
  xName: String;
begin
  xName := LabeledEdit5.Text;
  FDocument.Params.ValueByName[xName].AsString := Memo.Lines.Text;
  SetDocumentShowGrid;
end;

procedure TDocumentForm.ButtonAddField3Click(Sender: TObject);
var
  xName: String;
begin
  xName := LabeledEdit5.Text;
  FDocument.Params.ValueByName[xName].AsDate := DateTimePicker1.Date;
  SetDocumentShowGrid;
end;

procedure TDocumentForm.ButtonAddField4Click(Sender: TObject);
var
  xName: String;
begin
  xName := LabeledEdit5.Text;
  FDocument.Params.ValueByName[xName].AsTime := DateTimePicker2.Time;
  SetDocumentShowGrid;
end;

procedure TDocumentForm.ButtonAddField5Click(Sender: TObject);
var
  xName: String;
begin
  xName := LabeledEdit5.Text;
  FDocument.Params.ValueByName[xName].AsDateTime := DateTimePicker2.DateTime;
  SetDocumentShowGrid;
end;

procedure TDocumentForm.ButtonAddField6Click(Sender: TObject);
var
  xName: String;
begin
  xName := LabeledEdit5.Text;
  FDocument.Params.ValueByName[xName].AsInteger := UpDown1.Position;
  SetDocumentShowGrid;
end;

procedure TDocumentForm.ButtonAddField7Click(Sender: TObject);
var
  xName: String;
begin
  xName := LabeledEdit5.Text;
  FDocument.Params.ValueByName[xName].AsDouble := StrToFloatDef(LabeledEdit3.Text,148.56);
  SetDocumentShowGrid;
end;

procedure TDocumentForm.ButtonAddField8Click(Sender: TObject);
var
  xName: String;
begin
  xName := LabeledEdit5.Text;
  FDocument.Params.ValueByName[xName].AsBoolean := CheckBox1.Checked;
  SetDocumentShowGrid;
end;

procedure TDocumentForm.ButtonAddField9Click(Sender: TObject);
var
  xName: String;
  xFileName: String;
  xMS: TMemoryStream;
begin
  LabeledEdit4.Text := '';
  xName := LabeledEdit5.Text;
  xMS := TMemoryStream.Create;
  try
    if OpenDialog.Execute then
    begin
      xFileName := OpenDialog.FileName;
      LabeledEdit4.Text := xFileName;
      xMS.LoadFromFile(xFileName);
      FDocument.Params.ValueByName[xName].SetValueStream(xMS);
      SetDocumentShowGrid;
    end;
  finally
    FreeAndNil(xMS);
  end;
end;

end.
