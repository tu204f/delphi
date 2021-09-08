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
  Lb.Create.DB;

type
  ///<summary>Тип поля</summary>
  TFieldFrame = class(TFrame)
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TCrField read FField write SetField;
  end;

implementation

{$R *.fmx}

{ TFieldFrame }

constructor TFieldFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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

end.
