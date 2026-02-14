package upickle.jsonschema

import com.fasterxml.jackson.databind.ObjectMapper
import com.networknt.schema.{JsonSchemaFactory, SpecVersion}
import utest.*
import utest.framework.GoldenFix
import java.nio.file.Path

object SchemaSnapshotTestUtils {
  private val mapper = ObjectMapper()
  private val schemaFactory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V202012)

  private def goldenPath(resourcePath: String): Path = {
    val root = sys.env.getOrElse("MILL_TEST_RESOURCE_DIR", {
      throw new IllegalArgumentException("MILL_TEST_RESOURCE_DIR is not set")
    })
    val path = Path.of(root).resolve(resourcePath)
    if (!java.nio.file.Files.exists(path)) {
      throw new IllegalArgumentException(s"Missing golden resource: $path")
    }
    path
  }

  def assertSchemaSerializationCase[T](
      resourcePath: String,
      value: T,
      expectedSerializedJson: String,
      invalidJson: String,
      expectedInvalidError: String
  )(using JsonSchema[T], upickle.default.Writer[T], GoldenFix.Reporter): Unit = {
    val renderedSchema = JsonSchema.schemaFor[T](upickle.default).render(indent = 2)
    assertGoldenFile(renderedSchema, goldenPath(resourcePath))

    val serialized = upickle.default.write(value)
    assert(serialized == expectedSerializedJson)

    val schema = schemaFactory.getSchema(mapper.readTree(renderedSchema))
    val validErrors = schema.validate(mapper.readTree(serialized))
    assert(validErrors.isEmpty)
    val invalidErrors = schema.validate(mapper.readTree(invalidJson))
    assert(!invalidErrors.isEmpty)
    val invalidMessage = invalidErrors.toString
    assert(invalidMessage.nonEmpty)
    assert(invalidMessage.contains(expectedInvalidError))
  }

  def assertSerializationValidatesSchema[T](
      value: T,
      expectedSerializedJson: String
  )(using JsonSchema[T], upickle.default.Writer[T]): Unit = {
    val renderedSchema = JsonSchema.schemaFor[T](upickle.default).render(indent = 2)
    val serialized = upickle.default.write(value)
    assert(serialized == expectedSerializedJson)

    val schema = schemaFactory.getSchema(mapper.readTree(renderedSchema))
    val validationErrors = schema.validate(mapper.readTree(serialized))
    assert(validationErrors.isEmpty)
  }

  def assertJsonDoesNotValidateSchema[T](
      json: String
  )(using JsonSchema[T]): Unit = {
    val renderedSchema = JsonSchema.schemaFor[T](upickle.default).render(indent = 2)
    val schema = schemaFactory.getSchema(mapper.readTree(renderedSchema))
    val validationErrors = schema.validate(mapper.readTree(json))
    assert(!validationErrors.isEmpty)
  }

  def assertJsonDoesNotValidateSchemaWithMessage[T](
      json: String,
      expectedError: String
  )(using JsonSchema[T]): Unit = {
    val renderedSchema = JsonSchema.schemaFor[T](upickle.default).render(indent = 2)
    val schema = schemaFactory.getSchema(mapper.readTree(renderedSchema))
    val validationErrors = schema.validate(mapper.readTree(json))
    assert(!validationErrors.isEmpty)
    val renderedErrors = validationErrors.toString
    assert(renderedErrors.nonEmpty)
    assert(renderedErrors.contains(expectedError))
  }
}
