-- PRIMEIRA PARTE
SELECT DISTINCT PCEMBALAGEM.codauxiliar                                                                
       ,PCEMBALAGEM.CODPROD                                                                           
       ,PCEMBALAGEM.CODFILIAL                                                                         
       ,PCEMBALAGEM.EMBALAGEM                                                                         
       ,PCEMBALAGEM.UNIDADE                                                                           
       ,PCEMBALAGEM.QTUNIT                                                                            
       ,PCFORNEC.CODFORNEC                                                                            
       ,PCEMBALAGEM.DTEMISSAOETIQ                                                                     
       ,PCFORNEC.FORNECEDOR                                                                           
       ,PCFORNEC.FANTASIA                                                                             
       ,PCEMBALAGEM.QTMULTIPLA                                                                        
       ,PCEMBALAGEM.QTMAXGONDOLA                                                                      
       ,PCEMBALAGEM.QTMINGONDOLA                                                                      
       ,PCEMBALAGEM.QTEMISSAOETIQ                                                                     
,PCEMBALAGEM.ALTURA                                                                                   
,PCEMBALAGEM.LARGURA                                                                                  
,PCEMBALAGEM.COMPRIMENTO                                                                              
,PCEMBALAGEM.VOLUME                                                                                   
, TO_CHAR(PCEMBALAGEM.CODAUXILIAR, '00000000') CODBARRAS 
     ,N.CODAUXILIAR CODAUXILIARMENOR                                                
     ,N.QTUNIT QTUNITMENOR                                                          
     ,N.EMBALAGEM EMBALAGEMMENOR                                                    
     ,N.UNIDADE UNIDADEMENOR                                                        
,ROUND(COLUNA_PRECO(N.PRECO,'PVENDA'),2) PRECOEMBMENOR                                                
,COLUNA_PRECO(N.PRECO,'PVENDA')/DECODE(NVL(N.QTUNIT,0),0,1,N.QTUNIT) PRECOEMBMENORUNITARIO            
,(COLUNA_PRECO(N.PRECO,'PVENDA')/DECODE(NVL(N.QTUNIT,0),0,1,N.QTUNIT)) * M.QTUNIT PRECOEMBMENORNAEMBMAIOR
       ,ROUND(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDA')/ decode(NVL(PCEMBALAGEM.FATORCONVERSAO,1),0,1,NVL(PCEMBALAGEM.FATORCONVERSAO,1)) ,2) FATORCONVERSAO          
       ,ROUND(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC')/ decode(NVL(PCEMBALAGEM.FATORCONVERSAO,1),0,1,NVL(PCEMBALAGEM.FATORCONVERSAO,1)) ,2) FATORCONVERSAOATAC  
       ,PCEMBALAGEM.UNMEDIDA                                                                                                                                       
       ,ROUND((NVL(PCEMBALAGEM.QTMINIMAATACADO ,0) * COLUNA_PRECO(TBLPRECO.PRECO,'PVENDA')),2) PVENDAQTMINIMAATACADO  
       ,ROUND((NVL(PCEMBALAGEM.QTMINIMAATACADO ,0) * COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC')),2) PVENDAATACQTMINIMAATACADO 
       ,PCEST.QTESTGER                                                                                
       ,PCEMBALAGEM.PRECOANTERIOR                                                                     
       ,ROUND(CASE WHEN (ROUND(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC'), 2) <> ROUND(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDA'), 2)) or (:APLICAVAREJO = 'S' AND :APLICAATAC = 'S') 
                   THEN ROUND(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDA')                              
                        * (SELECT case when NVL(pctribut.peracrescismopf,0) = 0 then 1             
                                  else (1 + (NVL(pctribut.peracrescismopf, 1) / 100)) end          
                             FROM pctribut                                                         
                WHERE codst = (SELECT codst
                                FROM pctabtrib
                                WHERE codprod = pcprodut.codprod
                                AND PCTABTRIB.CODFILIALNF = PCEMBALAGEM.CODFILIAL
                                AND PCTABTRIB.UFDESTINO = (SELECT UF FROM PCFILIAL WHERE CODIGO=PCEMBALAGEM.CODFILIAL)))) 
            ELSE                                                       
                 ROUND(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDA'), 2)     
       END,2) PVENDAACRESCIMO                                             
      ,ROUND(CASE WHEN (TRUNC(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC'), 2) <> ROUND(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDA'), 2)) or (:APLICAVAREJO = 'S' AND :APLICAATAC = 'S') THEN 
                 ROUND(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC'),2)                                                           
                  * (SELECT case when NVL(pctribut.peracrescismopf,0) = 0                                                    
                                 then 1                                                                                      
                                 else (1 + (NVL (pctribut.peracrescismopf, 1)/100))                                          
                            end                                                                                              
                       FROM pctribut                                                                                         
                   WHERE codst = (SELECT codst                                                                            
                                    FROM pctabtrib                                                                        
                                   WHERE codprod = pcprodut.codprod                                                       
                                         AND PCTABTRIB.CODFILIALNF = PCEMBALAGEM.CODFILIAL                                
                                         AND PCTABTRIB.UFDESTINO = (SELECT UF                                             
                                                                      FROM PCFILIAL                                       
                                                                     WHERE CODIGO=PCEMBALAGEM.CODFILIAL)))             
            ELSE                                                                                                             
                 ROUND(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC'), 2)                                                       
       END,2) PVENDAATACACRESCIMO                                                                                            
      ,ROUND(CASE WHEN (TRUNC(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC'), 2) <> TRUNC(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDA'), 2)) or (:APLICAVAREJO = 'S' AND :APLICAATAC = 'S') 
                  THEN COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC')                                                          
                       * (SELECT (1 + (NVL (pctribut.peracrescismopf, 1) / 100) )                                                 
                            FROM pctribut                                                                                         
                        WHERE codst = (SELECT codst
                                         FROM pctabtrib
                                        WHERE codprod = pcprodut.codprod
                                              AND PCTABTRIB.CODFILIALNF = PCEMBALAGEM.CODFILIAL
                                              AND PCTABTRIB.UFDESTINO = (SELECT UF FROM PCFILIAL WHERE CODIGO=PCEMBALAGEM.CODFILIAL) ))
                  ELSE COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC')     
             END * NVL(PCEMBALAGEM.QTMINIMAATACADO,1), 2) PVENDAEMBALAGEMACRESCIMO 
       ,(NVL(PCEMBALAGEM.QTMULTIPLA,1) * TRUNC(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC'),2)) PVENDAQTMULTIPLA 
       ,PCEMBALAGEM.LAYOUTETIQUETA 
       ,ROUND(NVL(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDA'), 0),2) PVENDA  
       ,(SELECT PCBARRA.CODBARRA FROM PCBARRA 
          WHERE PCBARRA.CODPROD = PCPRODUT.CODPROD 
            AND PCBARRA.DTEXCLUSAO IS NULL 
            AND ROWNUM = 1) CODBARRAMASTER 
       ,ROUND(PCEMBALAGEM.PTABELA,2) PTABELA
       ,SYSDATE DATA 
       ,PCPRODUT.DV 
       ,NVL(RTRIM(LTRIM(PCEMBALAGEM.DESCRICAOECF)),PCPRODUT.DESCRICAO) DESCRICAO
       ,PCEMBALAGEM.DESCRICAOECF 
       ,PCPRODUT.CODFAB 
       ,PCPRODUT.DESCRICAO1 
       ,PCPRODUT.DESCRICAO2 
       ,PCPRODUT.codauxiliar2 
       ,PCPRODUT.PRAZOVAL 
       ,PCPRODUT.CODPRODMASTER 
       ,PCPRODUT.EMBALAGEMMASTER 
       ,PCPRODUT.CODEPTO
       ,PCPRODUT.CODSEC
       ,PCPRODUT.CODCATEGORIA
       ,PCEMBALAGEM.POFERTAATAC 
      ,(SELECT NVL(I.VLOFERTA,0)                                      
       FROM PCOFERTAPROGRAMADAC C, PCOFERTAPROGRAMADAI I              
       WHERE C.CODOFERTA = I.CODOFERTA                                
         AND C.CODFILIAL = PCEMBALAGEM.CODFILIAL                      
         AND I.CODFILIAL = PCEMBALAGEM.CODFILIAL                      
         AND I.CODAUXILIAR = PCEMBALAGEM.CODAUXILIAR                  
         AND C.DTCANCEL IS NULL                                       
         AND i.DATAEXCLUSAO IS NULL                                   
         AND TRUNC(SYSDATE) between c.dtinicial and C.DTFINAL         
 	AND ROWNUM=1) POFERTA2011                                          
      ,(SELECT NVL(I.VLOFERTAATAC ,0)                                 
       FROM PCOFERTAPROGRAMADAC C, PCOFERTAPROGRAMADAI I              
       WHERE C.CODOFERTA = I.CODOFERTA                                
         AND C.CODFILIAL = PCEMBALAGEM.CODFILIAL                      
         AND I.CODFILIAL = PCEMBALAGEM.CODFILIAL                      
         AND I.CODAUXILIAR = PCEMBALAGEM.CODAUXILIAR                  
         AND C.DTCANCEL IS NULL                                       
         AND i.DATAEXCLUSAO IS NULL                                   
         AND TRUNC(SYSDATE) between c.dtinicial and C.DTFINAL         
 	AND ROWNUM=1) POFERTAATAC2011                                      
      ,(SELECT C.DTINICIAL                                            
       FROM PCOFERTAPROGRAMADAC C, PCOFERTAPROGRAMADAI I              
       WHERE C.CODOFERTA = I.CODOFERTA                                
         AND C.CODFILIAL = PCEMBALAGEM.CODFILIAL                      
         AND I.CODFILIAL = PCEMBALAGEM.CODFILIAL                      
         AND I.CODAUXILIAR = PCEMBALAGEM.CODAUXILIAR                  
         AND C.DTCANCEL IS NULL                                       
         AND i.DATAEXCLUSAO IS NULL                                   
         AND TRUNC(SYSDATE) between c.dtinicial and C.DTFINAL         
 	AND ROWNUM=1 ) DTOFERTAINI2011                                     
      ,(SELECT C.DTFINAL                                              
       FROM PCOFERTAPROGRAMADAC C, PCOFERTAPROGRAMADAI I              
       WHERE C.CODOFERTA = I.CODOFERTA                                
         AND C.CODFILIAL = PCEMBALAGEM.CODFILIAL                      
         AND I.CODFILIAL = PCEMBALAGEM.CODFILIAL                      
         AND I.CODAUXILIAR = PCEMBALAGEM.CODAUXILIAR                  
         AND C.DTCANCEL IS NULL                                       
         AND i.DATAEXCLUSAO IS NULL                                   
         AND TRUNC(SYSDATE) between c.dtinicial and C.DTFINAL         
 	AND ROWNUM=1 ) DTOFERTAFIM2011                                     
       ,PCEMBALAGEM.POFERTA 
       ,PCEMBALAGEM.dtofertaini 
       ,PCEMBALAGEM.dtofertafim 
       ,PCEMBALAGEM.dtofertaATACini 
       ,PCEMBALAGEM.dtofertaATACfim 
       ,ROUND(PCEMBALAGEM.PTABELAATAC,2) PTABELAATAC
       ,ROUND(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC'),2) PVENDAATAC   
      ,M.EMBALAGEM EMBALAGEMMAIOR
      ,PCEMBALAGEM.QTMINIMAATACADO 
      ,M.QTMINIMAATACADO QTMINIMAATACADOMAIOR
      ,M.CODAUXILIAR CODAUXILIARMAIOR        
      ,M.UNIDADE UNIDADEMAIOR
      ,ROUND(COLUNA_PRECO(TBLPRECOMAIOR.PRECO,'PVENDAATAC'),2) PVENDAATACMAIOR 
      ,ROUND(COLUNA_PRECO(TBLPRECOMAIOR.PRECO,'PVENDA'),2) PVENDAMAIOR 
      ,M.QTUNIT QTUNITMAIOR
      ,PCPRODUT.MODULO 
      ,PCPRODUT.RUA 
      ,PCPRODUT.NUMERO 
      ,PCPRODUT.APTO 
      ,ROUND((COLUNA_PRECO(TBLPRECOMAIOR.PRECO,'PVENDA') /decode(nvl(M.qtunit,1),0,1,nvl(M.qtunit,1))),2) PVENDA_DIVIDIDO_QT_MAIOR 
      ,PCENDERECO.CODENDERECO 
      ,PCENDERECO.DEPOSITO 
      ,PCENDERECO.RUA PCENDERECORUA 
      ,PCENDERECO.PREDIO 
      ,PCENDERECO.NIVEL 
      ,PCENDERECO.APTO PCENDERECOAPTO 
      ,(SELECT TO_CHAR (codauxiliar) 
          FROM PCEMBALAGEM pcem 
         WHERE pcem.qtunit = 1 
           AND pcem.codprod = pcprodut.codprod 
           AND ROWNUM = 1) codauxilarqtunit1 
      ,PCPRODUT.UNIDADE UNIDADEPCPRODUT 
      ,PCPRODUT.UNIDADEMASTER UNIDADEMASTERPCPRODUT 
      ,PCPRODUT.NUMORIGINAL            
      ,PCPRODUT.EMBALAGEM EMBALAGEMPRODUTO 
      ,PCMARCA.CODMARCA 
      ,PCMARCA.MARCA 
      ,TO_CHAR(SUBSTR(PCPRODUT.DADOSTECNICOS,1,4000)) DADOSTECNICOS 
      ,PCPRODUT.INFORMACOESTECNICAS   
      ,PCPRODUT.DESCRICAO DESCPRODUTO 
      ,TBLVALIDADEEMB.DATAVALIDADE1           
      ,TBLVALIDADEEMB.DATAVALIDADE2           
      ,TBLVALIDADEEMB.DATAVALIDADE3           
      ,TBLVALIDADEEMB.DATAVALIDADE4           
      ,TBLVALIDADEEMB.DATAVALIDADE5           
      ,TBLVALIDADEEMB.DATAVALIDADE6           
      ,TBLVALIDADEEMB.DATAVALIDADE7           
      ,TBLVALIDADEEMB.DATAVALIDADE8           
      ,TBLVALIDADEEMB.DATAVALIDADE9           
      ,TBLVALIDADEEMB.DATAVALIDADE10          
      ,TBLTRIBUTACAO.PERCTRIBUTOS             
      ,NVL(NCM.percentfisica,0)   PERCTRIBUTOSNCMPF            
      ,NVL(NCM.percentjuridica,0) PERCTRIBUTOSNCMPJ            
      /* Pessoa Física e nacional */                                                        
      ,NVL(NCM.PERCENTFISICA,0)            PERCTRIBUTOSNCMFISICAFEDNAC        -- federal    
      ,NVL(NCM.PERCFISICAESTNAC,0)         PERCTRIBUTOSNCMFISICAESTNAC        -- estadual   
      ,NVL(NCM.PERCFISICAMUNICNAC,0)       PERCTRIBUTOSNCMFISICAMUNICNAC      -- municipal  
      /* Pessoa física e importado */                                                       
      ,NVL(NCM.PERCENTFISICAIMPORTADO,0)   PERCTRIBUTOSNCMFISICAFEDIMP        -- federal    
      ,NVL(NCM.PERCFISICAESTIMP,0)         PERCTRIBUTOSNCMFISICAESTIMP        -- estadual   
      ,NVL(NCM.PERCFISICAMUNICIMP,0)       PERCTRIBUTOSNCMFISICAMUNICIMP      -- municipal  
      /* Pessoa juridia e nacional */                                                       
      ,NVL(NCM.PERCENTJURIDICA,0)          PERCTRIBUTOSNCMJURIDICFEDNAC       -- federal    
      ,NVL(NCM.PERCJURIDICESTNAC,0)        PERCTRIBUTOSNCMJURIDICESTNAC       -- estadual   
      ,NVL(NCM.PERCJURIDICMUNICNAC,0)      PERCTRIBUTOSNCMJURIDICMUNICNAC     -- municipal  
      /* Pessoa juridica e importado */                                                     
      ,NVL(NCM.PERCENTJURIDICAIMPORTADO,0) PERCTRIBUTOSNCMJURIDICFEDIMP       -- federal    
      ,NVL(NCM.PERCJURIDICESTIMP,0)        PERCTRIBUTOSNCMJURIDICESTIMP       -- estadual   
      ,NVL(NCM.PERCJURIDICMUNICIMP,0)      PERCTRIBUTOSNCMJURIDICMUNICIMP     -- municipal  
      ,NVL(PCEMBALAGEM.PVENDA, 0) AS PVENDA2017 
      ,NVL(PCEMBALAGEM.PVENDAATAC, 0) AS PVENDAATAC2017 
 FROM PCEMBALAGEM, PCPRODUT, PCFORNEC, PCENDERECO, PCMARCA, PCEMBALAGEM M, PCEST 
    , (SELECT CODFILIAL, CODNCM, PERCENTFISICA, PERCENTJURIDICA                  
            , PERCFISICAESTNAC, PERCFISICAMUNICNAC                               
            , PERCENTFISICAIMPORTADO, PERCFISICAESTIMP, PERCFISICAMUNICIMP       
            , PERCJURIDICESTNAC, PERCJURIDICMUNICNAC                             
            , PERCENTJURIDICAIMPORTADO, PERCJURIDICESTIMP, PERCJURIDICMUNICIMP   
         FROM PCTRIBNCMFILIAL A                                                  
        WHERE CODFILIAL = (SELECT MIN(CODFILIAL)                                 
                             FROM PCTRIBNCMFILIAL AA                             
                            WHERE CODNCM = A.CODNCM                              
                              AND CODFILIAL IN (:CODFILIAL, '99'))) NCM        
,(SELECT CODFILIAL                                                                
     ,CODPROD                                                                     
     ,CODAUXILIAR                                                                 
     ,QTUNIT                                                                      
     ,EMBALAGEM                                                                   
     ,UNIDADE                                                                     
     ,BUSCAPRECOS(PCEMBALAGEM.CODFILIAL, 0, PCEMBALAGEM.CODAUXILIAR, :DATAOFERTA) PRECO 
 FROM PCEMBALAGEM                                                              
 WHERE 1=1                                                                     
AND PCEMBALAGEM.CODFILIAL = :CODFILIAL
AND PCEMBALAGEM.CODPROD = :CODPRODUTO
AND PCEMBALAGEM.DTINATIVO IS NULL

-- SEGUNDA PARTE

                              ) N 
 ,(SELECT                                                
     codprod,                                           
     codauxiliar,                                       
     codfilial,                                         
     max(colun1) AS DATAVALIDADE1,                              
     max(colun2) AS DATAVALIDADE2,                              
     max(colun3) AS DATAVALIDADE3,                              
     max(colun4) AS DATAVALIDADE4,                              
     max(colun5) AS DATAVALIDADE5,                              
     max(colun6) AS DATAVALIDADE6,                              
     max(colun7) AS DATAVALIDADE7,                              
     max(colun8) AS DATAVALIDADE8,                              
     max(colun9) AS DATAVALIDADE9,                              
     max(colun10) AS DATAVALIDADE10                             
   FROM (                                               
     SELECT                                             
       v.codprod,                                       
       v.codauxiliar,                                   
       v.codfilial,                                     
       decode(numvalidade,1,dtvalidade,'') colun1,      
       decode(numvalidade,2,dtvalidade,'') colun2,      
       decode(numvalidade,3,dtvalidade,'') colun3,      
       decode(numvalidade,4,dtvalidade,'') colun4,      
       decode(numvalidade,5,dtvalidade,'') colun5,      
       decode(numvalidade,6,dtvalidade,'') colun6,      
       decode(numvalidade,7,dtvalidade,'') colun7,      
       decode(numvalidade,8,dtvalidade,'') colun8,      
       decode(numvalidade,9,dtvalidade,'') colun9,      
       decode(numvalidade,10,dtvalidade,'') colun10     
     FROM                                               
       PCVALIDADEPRODOFERTA V, PCEMBALAGEM, PCOFERTAPROGRAMADAC C, PCOFERTAPROGRAMADAI I 
     WHERE                                              
       V.CODFILIAL = PCEMBALAGEM.CODFILIAL                        
       AND V.CODAUXILIAR = PCEMBALAGEM.CODAUXILIAR                
       AND V.CODFILIAL = C.CODFILIAL                    
       AND V.CODAUXILIAR = I.CODAUXILIAR                
       AND C.CODOFERTA = I.CODOFERTA                    
       AND C.DTCANCEL IS NULL                           
       AND i.DATAEXCLUSAO IS NULL 
/* impressao de etiqueta */
 AND PCEMBALAGEM.CODPROD IN ( :CODPRODUTO)
       AND ((:DATAOFERTA BETWEEN PCEMBALAGEM.DTOFERTAINI AND PCEMBALAGEM.DTOFERTAFIM) OR     
            (:DATAOFERTA BETWEEN C.DTINICIAL AND C.DTFINAL))             
     )                                                                   
   GROUP BY CODPROD,CODAUXILIAR,CODFILIAL) TBLVALIDADEEMB                
 ,(SELECT PCEMBALAGEM.CODAUXILIAR                                                                   
           ,PCEMBALAGEM.CODFILIAL                                                                   
           ,PCEMBALAGEM.DTOFERTAINI                                                                 
           ,PCEMBALAGEM.DTOFERTAATACINI                                                             
           ,BUSCAPRECOS(PCEMBALAGEM.CODFILIAL, 0, PCEMBALAGEM.CODAUXILIAR, :DATAOFERTA) PRECO       
           ,PCEMBALAGEM.CODPROD                                                                     
    FROM PCEMBALAGEM                                                                                
    WHERE 1=1
/* impressao de etiqueta */
 AND PCEMBALAGEM.CODPROD IN ( :CODPRODUTO)
    ) TBLPRECO                                                                                      
 ,(SELECT PCEMBALAGEM.CODAUXILIAR                                                                   
           ,PCEMBALAGEM.CODFILIAL                                                                   
           ,PCEMBALAGEM.DTOFERTAINI                                                                 
           ,BUSCAPRECOS(PCEMBALAGEM.CODFILIAL, 0, PCEMBALAGEM.CODAUXILIAR, :DATAOFERTA) PRECO       
           ,PCEMBALAGEM.CODPROD                            
    FROM PCEMBALAGEM                                       
    WHERE 1=1
AND PCEMBALAGEM.CODFILIAL = :CODFILIAL
AND PCEMBALAGEM.CODPROD = :CODPRODUTO
AND PCEMBALAGEM.DTINATIVO IS NULL
    ) TBLPRECOMAIOR                                        
 ,(SELECT PCEMBALAGEM.CODFILIAL                                                                                         
        , PCEMBALAGEM.CODAUXILIAR                                                                                       
        , PCTRIBUT.CODST                                                                                                
        , PCTRIBUT.CODICMTAB                                                                                            
        , NVL(PCTRIBUT.PERCTRIBUTOS,0) PERCTRIBUTOS                                                                     
     FROM PCEMBALAGEM                                                                                                   
        , PCTRIBUT                                                                                                      
        , (SELECT CODFILIAL, VALOR FROM PCPARAMFILIAL WHERE NOME LIKE 'UTILIZAMARGEMSUBCAT') TBLUSAMARGEMSUBCAT       
        , (SELECT CODFILIAL, VALOR FROM PCPARAMFILIAL WHERE NOME LIKE 'UTILIZATRIBUTSUBCAT') TBLUSATRIBUTACAOSUBCAT   
        , (SELECT CODFILIAL, VALOR FROM PCPARAMFILIAL WHERE NOME LIKE 'CON_USATRIBUTACAOPORUF') TBLUSATRIBUTACAOPORUF 
        , (SELECT PCPARAMFILIAL.CODFILIAL                                                                               
                , NVL(PCPARAMFILIAL.VALOR, NVL(PCFILIAL.NUMREGIAOPADRAO,PCCONSUM.NUMREGIAOPADRAO)) NUMREGIAO            
             FROM PCPARAMFILIAL, PCFILIAL, PCCONSUM                                                                     
            WHERE PCPARAMFILIAL.CODFILIAL = PCFILIAL.CODIGO                                                             
              AND PCPARAMFILIAL.NOME LIKE 'NUMREGIAOPADRAOVAREJO') TBLNUMREGIAO                                       
        , (TABLE(BUSCAMARGEM(PCEMBALAGEM.CODFILIAL                                                                      
                             ,PCEMBALAGEM.CODPROD                                                                       
                             ,PCEMBALAGEM.CODAUXILIAR                                                                   
                             ,TBLNUMREGIAO.NUMREGIAO                                                                    
                             ,PCEMBALAGEM.PERVARIACAOPTABELA                                                            
                             ,TBLUSAMARGEMSUBCAT.VALOR                                                                  
                             ,TBLUSATRIBUTACAOSUBCAT.VALOR                                                              
                             ,TBLUSATRIBUTACAOPORUF.VALOR))) TBLMARGEM                                                  
    WHERE PCEMBALAGEM.CODAUXILIAR = TBLMARGEM.CODAUXILIAR                                                               
      AND PCEMBALAGEM.CODFILIAL = TBLMARGEM.CODFILIAL                                                                   
      AND PCEMBALAGEM.CODFILIAL = TBLUSAMARGEMSUBCAT.CODFILIAL                                                          
      AND PCEMBALAGEM.CODFILIAL = TBLUSATRIBUTACAOSUBCAT.CODFILIAL                                                      
      AND PCEMBALAGEM.CODFILIAL = TBLNUMREGIAO.CODFILIAL                                                                
      AND PCEMBALAGEM.CODFILIAL = :CODFILIAL                                                                            
/* impressao de etiqueta */
 AND PCEMBALAGEM.CODPROD IN ( :CODPRODUTO)
      AND PCTRIBUT.CODST = TBLMARGEM.CODST) TBLTRIBUTACAO                                                             
 WHERE PCEMBALAGEM.CODPROD = PCPRODUT.CODPROD              
  AND PCEMBALAGEM.CODPROD   = M.CODPROD                    
  AND PCEMBALAGEM.CODFILIAL = M.CODFILIAL                  
  AND NVL(PCEMBALAGEM.PERMITEIMPRESSAOETIQUETA,'S') = 'S'         
  AND PCFORNEC.CODFORNEC  = PCPRODUT.CODFORNEC             
  AND PCENDERECO.CODENDERECO(+) = PCPRODUT.CODENDERECOAP   
  AND PCPRODUT.CODMARCA = PCMARCA.CODMARCA(+)              
  AND PCEMBALAGEM.CODFILIAL     = TBLPRECO.CODFILIAL       
  AND PCEMBALAGEM.CODAUXILIAR   = TBLPRECO.CODAUXILIAR     
  AND PCEMBALAGEM.CODPROD       = TBLPRECO.CODPROD         
  AND M.CODFILIAL     = TBLPRECOMAIOR.CODFILIAL            
  AND M.CODAUXILIAR   = TBLPRECOMAIOR.CODAUXILIAR          
  AND M.CODPROD       = TBLPRECOMAIOR.CODPROD              
  AND PCEMBALAGEM.CODFILIAL = TBLVALIDADEEMB.CODFILIAL(+)  
  AND PCEMBALAGEM.CODAUXILIAR = TBLVALIDADEEMB.CODAUXILIAR(+) 
  AND PCEST.CODFILIAL = PCEMBALAGEM.CODFILIAL              
  AND PCEST.CODPROD = PCEMBALAGEM.CODPROD                  
  AND N.CODFILIAL = PCEMBALAGEM.codfilial
  AND N.CODAUXILIAR = (select min(e.codauxiliar)
                         from pcembalagem e
                        where e.codprod = pcembalagem.codprod
                              and e.codfilial = pcembalagem.codfilial
                              and e.dtinativo is null 
                              and nvl(e.permiteimpressaoetiqueta, 'S') = 'S' 
                              and e.qtunit = (select MIN(X.QTUNIT)
                                                 from PCEMBALAGEM X
                                                where X.CODPROD = pcembalagem.CODPROD
                                                      and X.CODFILIAL = PCEMBALAGEM.CODFILIAL and x.dtinativo is null
                                                      and nvl(x.permiteimpressaoetiqueta, 'S') = 'S'              
                      ))
  AND PCEMBALAGEM.CODFILIAL = TBLTRIBUTACAO.CODFILIAL(+)     
  AND PCEMBALAGEM.CODAUXILIAR = TBLTRIBUTACAO.CODAUXILIAR(+) 
  AND replace(PCPRODUT.NBM,'.','') = replace(NCM.codncm(+),'.','') 
/* impressao de etiqueta */
 AND PCEMBALAGEM.CODPROD IN ( :CODPRODUTO)
/* impressao de etiqueta */
 AND PCPRODUT.CODPROD IN ( :CODPRODUTO)
AND PCEMBALAGEM.DTINATIVO IS NULL         
AND PCEMBALAGEM.CODFILIAL = :CODFILIAL
AND (NVL(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDA'), 0) > 0 OR NVL(COLUNA_PRECO(TBLPRECO.PRECO,'PVENDAATAC'), 0) > 0)
AND PCPRODUT.CODPROD = :CODPRODUTO
    AND M.CODAUXILIAR = (select max(codauxiliar)                                                    
                           from pcembalagem e                                                       
                          where e.codprod = pcembalagem.codprod                                     
                                and e.codfilial = pcembalagem.codfilial                             
                                and nvl(e.permiteimpressaoetiqueta, 'S') = 'S'                  
                          and e.dtinativo is null                                             
                                and e.qtunit = (select MAX(X.QTUNIT)                                
                                                  from PCEMBALAGEM X                                
                                                 where X.CODPROD = M.CODPROD                        
                                                       and X.CODFILIAL = PCEMBALAGEM.CODFILIAL      
                                                       and nvl(X.permiteimpressaoetiqueta, 'S') = 'S' 
                                                 and x.dtinativo is null                      
                         ))                                                                         
ORDER BY PCPRODUT.CODEPTO, NVL(PCEMBALAGEM.DESCRICAOECF,PCPRODUT.DESCRICAO), PCEMBALAGEM.CODPROD, PCEMBALAGEM.QTUNIT;
