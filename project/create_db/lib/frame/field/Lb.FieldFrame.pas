unit Lb.FieldFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Objects,
  FMX.ListBox,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  Lb.Create.DB,
  Lb.SysUtils,
  Lb.WinFrame;

type
  ///<summary>Тип поля</summary>
  TFieldFrame = class(TFrame, IWinModule)
    GridPanelLayout: TGridPanelLayout;
    TextID: TText;
    EditID: TEdit;
    TextName: TText;
    EditFieldName: TEdit;
    TextFieldType: TText;
    ComboBoxFieldType: TComboBox;
    LayoutDescription: TLayout;
    TextDescription: TText;
    MemoDescription: TMemo;
    Layout: TLayout;
  private
    FField: TCrField;
    procedure SetField(const Value: TCrField);
  private
    FStatus: TStatusFrame;
    function GetCode: WideString;
    function GetStatus: TStatusFrame;
    procedure SetStatus(const AStatus: TStatusFrame);
  protected
    procedure SetApply;
    procedure SetClose;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TCrField read FField write SetField;
    property Code: WideString read GetCode;
    property Status: TStatusFrame read GetStatus write SetStatus;
  end;

implementation

{$R *.fmx}

{ TFieldFrame }

constructor TFieldFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Align := TAlignLayout.Client;
  FField := nil;
  TSysConfig.SetTypeFields(ComboBoxFieldType.Items);
end;

destructor TFieldFrame.Destroy;
begin

  inherited;
end;

procedure TFieldFrame.SetField(const Value: TCrField);
begin
  FField := Value;
  EditID.Text := FField.ObjectKey;
//  EditFieldName.Text := FField.Name;
//  ComboBoxFieldType.ItemIndex := TSysConfig.GetIndexOfTypeField(FField.TypeField);
//  MemoDescription.Text := FField.Description;
end;

function TFieldFrame.GetCode: WideString;
begin
  Result := Self.ClassName;
end;

function TFieldFrame.GetStatus: TStatusFrame;
begin
  Result := FStatus;
end;

procedure TFieldFrame.SetStatus(const AStatus: TStatusFrame);
begin
  FStatus := AStatus;
end;

procedure TFieldFrame.SetApply;
begin
  // Применить изменение
end;

procedure TFieldFrame.SetClose;
begin
  // Отменить заявки
end;

end.
