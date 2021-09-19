unit Lb.ModuleTableFrame;

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
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Layouts,
  Lb.Create.DB,
  FMX.ListBox,
  FMX.Menus;

type
  ///<summary>Список модулей</summary>
  TModuleTableFrame = class(TFrame)
    Layout: TLayout;
    ListBox: TListBox;
    PopupMenu: TPopupMenu;
    MenuItemAdd: TMenuItem;
    MenuItemChange: TMenuItem;
    MenuItemDelete: TMenuItem;
    procedure MenuItemAddClick(Sender: TObject);
    procedure MenuItemChangeClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
  private
    FModules: TCrModules;
    procedure SetModules(const Value: TCrModules);
    procedure EventUpdataModuleParams(Sender: TObject; const AParams: TStrings);
  protected
    procedure SetUpDataModules;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Modules: TCrModules read FModules write SetModules;
  end;

implementation

{$R *.fmx}

uses
  Lb.Core.Events,
  Lb.SysUtils, Lb.ListBoxItem.Params;

{ TModulesFrame }

constructor TModuleTableFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModules := nil;
  ApplicationEvents.GetEvents(EVENT_MODULE_TABLE_UPDATA).EventStrings := EventUpdataModuleParams;
end;

destructor TModuleTableFrame.Destroy;
begin

  inherited;
end;

procedure TModuleTableFrame.SetModules(const Value: TCrModules);
begin
  FModules := Value;
  Self.SetUpDataModules;
end;

procedure TModuleTableFrame.MenuItemAddClick(Sender: TObject);
begin
  ApplicationEvents.SetEvent(EVENT_MODULE_ADD,Self);
end;

procedure TModuleTableFrame.MenuItemChangeClick(Sender: TObject);
begin
  ApplicationEvents.SetEvent(EVENT_MODULE_CHANGE,Self);
end;

procedure TModuleTableFrame.MenuItemDeleteClick(Sender: TObject);
begin
  ApplicationEvents.SetEvent(EVENT_MODULE_DELETED,Self);
end;

procedure TModuleTableFrame.EventUpdataModuleParams(Sender: TObject; const AParams: TStrings);
begin
  Self.SetUpDataModules;
end;

procedure TModuleTableFrame.ListBoxDblClick(Sender: TObject);
begin
  var xItem := TListBoxItemParams(ListBox.Selected);
  if Assigned(xItem) then
    ApplicationEvents.SetEvent(EVENT_MODULE_TABLE_DLCLICK,Self,xItem.Strings);
end;

procedure TModuleTableFrame.SetUpDataModules;
var
  xS: String;
  xModule: TCrModule;
begin
  if Assigned(FModules) then
  begin
    ListBox.Items.Clear;
    for xModule in FModules do
    begin
      var xItem := TListBoxItemParams.Create(ListBox);
      xItem.Text := xModule.Name;
      xItem.Params['object_key'] := xModule.ObjectKey;
      xItem.Params['name']       := xModule.Name;
      xItem.Parent := ListBox;
    end;
  end;
end;

end.
