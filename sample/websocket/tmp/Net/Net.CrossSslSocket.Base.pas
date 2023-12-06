unit Net.CrossSslSocket.Base;

interface

{$I zLib.inc}

uses
  SysUtils,
  Classes,

  Net.CrossSocket.Base,
  Net.CrossSocket,

  Utils.IOUtils;

type
  ICrossSslConnection = interface(ICrossConnection)
  ['{7B7B1DE2-8EDE-4F10-8193-2769D29C59FB}']
    function GetSsl: Boolean;

    /// <summary>
    ///   �Ƿ������� SSL
    /// </summary>
    property Ssl: Boolean read GetSsl;
  end;

  /// <summary>
  ///   SSL Socket
  /// </summary>
  /// <remarks>
  ///   ��ȷ��ʹ�ò���:
  ///   <list type="number">
  ///     <item>
  ///       SetCertificateificate �� SetCertificateificateFile
  ///     </item>
  ///     <item>
  ///       SetPrivateKey �� SetPrivateKeyFile, �ͻ��˲���Ҫ��һ��
  ///     </item>
  ///     <item>
  ///       Connect / Listen
  ///     </item>
  ///   </list>
  /// </remarks>
  ICrossSslSocket = interface(ICrossSocket)
  ['{A4765486-A0F1-4EFD-BC39-FA16AED21A6A}']
    function GetSsl: Boolean;

    /// <summary>
    ///   ���ڴ����֤��
    /// </summary>
    /// <param name="ACertBuf">
    ///   ֤�黺����
    /// </param>
    /// <param name="ACertBufSize">
    ///   ֤�黺������С
    /// </param>
    procedure SetCertificate(const ACertBuf: Pointer; const ACertBufSize: Integer); overload;

    /// <summary>
    ///   ���ַ�������֤��
    /// </summary>
    /// <param name="ACertStr">
    ///   ֤���ַ���
    /// </param>
    procedure SetCertificate(const ACertStr: string); overload;

    /// <summary>
    ///   ���ļ�����֤��
    /// </summary>
    /// <param name="ACertFile">
    ///   ֤���ļ�
    /// </param>
    procedure SetCertificateFile(const ACertFile: string);

    /// <summary>
    ///   ���ڴ����˽Կ
    /// </summary>
    /// <param name="APKeyBuf">
    ///   ˽Կ������
    /// </param>
    /// <param name="APKeyBufSize">
    ///   ˽Կ��������С
    /// </param>
    procedure SetPrivateKey(const APKeyBuf: Pointer; const APKeyBufSize: Integer); overload;

    /// <summary>
    ///   ���ַ�������˽Կ
    /// </summary>
    /// <param name="APKeyStr">
    ///   ˽Կ�ַ���
    /// </param>
    procedure SetPrivateKey(const APKeyStr: string); overload;

    /// <summary>
    ///   ���ļ�����˽Կ
    /// </summary>
    /// <param name="APKeyFile">
    ///   ˽Կ�ļ�
    /// </param>
    procedure SetPrivateKeyFile(const APKeyFile: string);

    /// <summary>
    ///   �Ƿ������� SSL
    /// </summary>
    property Ssl: Boolean read GetSsl;
  end;

  TCrossSslListenBase = class(TCrossListen);

  TCrossSslConnectionBase = class(TCrossConnection, ICrossSslConnection)
  protected
    function GetSsl: Boolean;
  public
    property Ssl: Boolean read GetSsl;
  end;

  TCrossSslSocketBase = class(TCrossSocket, ICrossSslSocket)
  private
    FSsl: Boolean;
  protected
    function GetSsl: Boolean;
  public
    constructor Create(const AIoThreads: Integer; const ASsl: Boolean); reintroduce; virtual;

    procedure SetCertificate(const ACertBuf: Pointer; const ACertBufSize: Integer); overload; virtual; abstract;
    procedure SetCertificate(const ACertBytes: TBytes); overload; virtual;
    procedure SetCertificate(const ACertStr: string); overload; virtual;
    procedure SetCertificateFile(const ACertFile: string); virtual;

    procedure SetPrivateKey(const APKeyBuf: Pointer; const APKeyBufSize: Integer); overload; virtual; abstract;
    procedure SetPrivateKey(const APKeyBytes: TBytes); overload; virtual;
    procedure SetPrivateKey(const APKeyStr: string); overload; virtual;
    procedure SetPrivateKeyFile(const APKeyFile: string); virtual;

    property Ssl: Boolean read GetSsl;
  end;

implementation

{ TCrossSslSocketBase }

constructor TCrossSslSocketBase.Create(const AIoThreads: Integer;
  const ASsl: Boolean);
begin
  inherited Create(AIoThreads);

  FSsl := ASsl;
end;

function TCrossSslSocketBase.GetSsl: Boolean;
begin
  Result := FSsl;
end;

procedure TCrossSslSocketBase.SetCertificate(const ACertBytes: TBytes);
begin
  SetCertificate(Pointer(ACertBytes), Length(ACertBytes));
end;

procedure TCrossSslSocketBase.SetCertificate(const ACertStr: string);
begin
  SetCertificate(TEncoding.ANSI.GetBytes(ACertStr));
end;

procedure TCrossSslSocketBase.SetCertificateFile(const ACertFile: string);
begin
  SetCertificate(TFileUtils.ReadAllBytes(ACertFile));
end;

procedure TCrossSslSocketBase.SetPrivateKey(const APKeyBytes: TBytes);
begin
  SetPrivateKey(Pointer(APKeyBytes), Length(APKeyBytes));
end;

procedure TCrossSslSocketBase.SetPrivateKey(const APKeyStr: string);
begin
  SetPrivateKey(TEncoding.ANSI.GetBytes(APKeyStr));
end;

procedure TCrossSslSocketBase.SetPrivateKeyFile(const APKeyFile: string);
begin
  SetPrivateKey(TFileUtils.ReadAllBytes(APKeyFile));
end;

{ TCrossSslConnectionBase }

function TCrossSslConnectionBase.GetSsl: Boolean;
begin
  Result := TCrossSslSocketBase(Owner).Ssl;
end;

end.
