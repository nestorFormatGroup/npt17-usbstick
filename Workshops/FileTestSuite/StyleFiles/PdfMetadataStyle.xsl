<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/">
<html>
<head>
<style>
tr.captiondred {background-color: #FF0000}
tr.captionred {background-color: #CD5C5C}
tr.captiongreen {background-color: #006400}
tr.captiontan {background-color: #FFDEAD}
tr.captionkhaki {background-color: #BDB76B}
tr.captionolive {background-color: #808000}
tr.captionwheat {background-color: #F5DEB3}
</style>
</head>
<body>
<h2>Pdf Metadata Extraction</h2>
<table border ="1">
<tr class="captiontan">
<th>File Name</th> 
<th>No. of Metadata Entries</th> 
<th>Year of Creation</th> 
<th>Creation Date</th> 
<th>Modification Date</th> 
<th>Title</th> 
<th>Author</th> 
</tr>
<xsl:for-each select="PdfMetadata/File">
<xsl:sort select="CreationYear" />
<tr class="captiontan">
<td><xsl:value-of select="FileName"/></td>
<td><xsl:value-of select="MetadataEntries"/></td>
<td><xsl:value-of select="CreationYear"/></td>
<td><xsl:value-of select="CreationDate"/></td>
<td><xsl:value-of select="ModificationDate"/></td>
<td><xsl:value-of select="Title"/></td>
<td><xsl:value-of select="Author"/></td>
</tr>
</xsl:for-each>
</table>
</body>
</html>
</xsl:template>
</xsl:stylesheet>
