unit Lb.TreeViewItem.Params;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants,
  FMX.Forms,
  FMX.Types,
  FMX.TreeView;

type
  TTreeViewItemParams = class(TTreeViewItem)
  private
    FFrame: TFrame;
    FParams: TStrings;
    function GetParams(Name: String): String;
    procedure SetParams(Name: String; const Value: String);
    procedure SetFrame(const Value: TFrame);
    function GetTextParams: String;
  protected
    function GetDefaultStyleLookupName: string; override;
    procedure SetUpdateParam; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Frame: TFrame read FFrame write SetFrame;
    property Params[Name: String]: String read GetParams write SetParams;
    property TextParams: String read GetTextParams;
  end;

implementation

{ TTreeViewItemParams }

constructor TTreeViewItemParams.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TStringList.Create;
  FFrame := nil;
end;

destructor TTreeViewItemParams.Destroy;
begin
  if Assigned(FFrame) then
    FreeAndNil(FFrame);
  FreeAndNil(FParams);
  inherited;
end;

function TTreeViewItemParams.GetDefaultStyleLookupName: string;
begin
  Result := 'TreeViewItemstyle'
end;

function TTreeViewItemParams.GetParams(Name: String): String;
begin
  Result := FParams.Values[Name];
end;

function TTreeViewItemParams.GetTextParams: String;
begin
  Result := FParams.Text;
end;

procedure TTreeViewItemParams.SetFrame(const Value: TFrame);
begin
  FFrame := Value;
  if Assigned(FFrame) then
  begin
    FFrame.Parent := Self;
    FFrame.Align := TAlignLayout.Client;
    FFrame.Margins.Left := 10;
  end;
end;

procedure TTreeViewItemParams.SetParams(Name: String; const Value: String);
begin
  FParams.Values[Name] := Value;
end;

procedure TTreeViewItemParams.SetUpdateParam;
begin
end;

end.
